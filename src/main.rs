#![allow(dead_code)]

use core::panic;
use std::{
    collections::HashSet,
    num::NonZeroU8,
    ops::{Index, IndexMut},
};

static ERROR_SELF_REFERENTIAL: &str = "#SELFREF";
static ERROR_DATA_TYPE: &str = "#DATATYPE";
static ERROR_NOT_ENOUGH_ARGUMENTS: &str = "#NUM_ARGS";
static ERROR_NO_VALUE_LEFT_ON_STACK: &str = "#NO_VALUE";
static ERROR_RESIDUAL_OPERATOR: &str = "#RESID_OP";
static ERROR_DIVISION_BY_ZERO: &str = "#DIV_BY_0";

static SPLITTABLE_TOKENS: &[char] = &['+', '-', '*', '/', '%', '$', '&', '^'];
static SPLITTABLE_TOKENS_SPACE: &[char] = &['+', '-', '*', '/', '%', '$', '&', '^', ' '];

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref CELL_REGEX: Regex = Regex::new("[A-Z]+[1-9]+[0-9]*").unwrap();
}

use yacurses::*;

static LINEN_RED: ColorID = ColorID(0x11);
static LINEN_BG_ACTIVE: ColorID = ColorID(0x12);
static LINEN_ACTIVE_PAIR: ColorPair = ColorPair(unsafe { NonZeroU8::new_unchecked(255) }); // SAFETY: trivial

type CursesRes = Result<(), Box<dyn std::error::Error>>;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
struct Coord(usize, usize); // (x, y)

#[derive(Debug)]
enum Token {
    Text(String),
    Number(f64),
    Reference(Coord, CellValue),

    Error(&'static str),

    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

#[derive(Clone, Debug)]
enum CellValue {
    Numerical(f64),     // a constant number
    Text(String),       // a constant string
    Expression(String), // a "formula"
    Empty,
}

impl std::fmt::Display for CellValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CellValue::Empty => String::from(""),
                CellValue::Numerical(val) => val.to_string(),
                CellValue::Text(val) => val.clone(),
                CellValue::Expression(val) => val.clone(),
            }
        )
    }
}

#[derive(Clone, Debug)]
struct SheetCell {
    coord: Coord,
    value: CellValue,
    precision: u8,
    referenced_by: HashSet<Coord>,
}

impl SheetCell {
    fn new(coord: Coord) -> Self {
        SheetCell {
            coord,
            value: CellValue::Empty,
            precision: 2,
            referenced_by: HashSet::new(),
        }
    }
}

#[derive(Clone)]
struct CellVec {
    rows: usize,
    cols: usize,
    data: Vec<SheetCell>,
}

impl Index<Coord> for CellVec {
    type Output = SheetCell;
    fn index(&self, index: Coord) -> &Self::Output {
        &self.data[self.rows * index.1 + index.0]
    }
}

impl IndexMut<Coord> for CellVec {
    fn index_mut(&mut self, index: Coord) -> &mut Self::Output {
        &mut self.data[self.rows * index.1 + index.0]
    }
}

fn letters_to_coord(input: &str) -> Coord {
    let letters = input
        .chars()
        .filter(|c| c.is_alphabetic())
        .collect::<String>();
    let maybe_numbers = input
        .chars()
        .filter(|c| c.is_numeric())
        .collect::<String>()
        .parse::<usize>();

    if maybe_numbers.is_err() {
        panic!("Could not parse the row.")
    }
    if letters.is_empty() {
        panic!("Could not parse the column.")
    }

    let numbers = maybe_numbers.unwrap();

    // TODO: support letter parts bigger than one letter
    let col = letters.chars().next().unwrap() as usize - 65;
    let row = numbers - 1;

    Coord(col, row) // strange switcheroo but necessary apparently
}

impl Index<&str> for CellVec {
    type Output = SheetCell;
    fn index(&self, index: &str) -> &Self::Output {
        &self[letters_to_coord(index)]
    }
}

impl IndexMut<&str> for CellVec {
    fn index_mut(&mut self, index: &str) -> &mut Self::Output {
        &mut self[letters_to_coord(index)]
    }
}

impl CellVec {
    fn new(rows: usize, cols: usize) -> Self {
        CellVec {
            rows,
            cols,
            data: (0..rows)
                .flat_map(move |x| std::iter::repeat(x).zip(0..cols))
                .map(|(r, c)| SheetCell::new(Coord(r, c)))
                .collect::<Vec<_>>(),
        }
    }

    fn get_all_dependents(&self, cell: Coord, path: Vec<Coord>) -> Vec<Coord> {
        let dependents = self[cell]
            .referenced_by
            .iter()
            .filter(|d| !path.contains(d))
            .into_iter()
            .collect::<Vec<_>>();
        if dependents.is_empty() {
            vec![cell]
        } else {
            dependents
                .iter()
                .map(|d| {
                    let mut new_path = path.clone();
                    new_path.push(cell);
                    self.get_all_dependents(**d, new_path)
                })
                .flatten()
                .collect()
        }
    }
}

enum Direction {
    Left,
    Right,
    Up,
    Down,
}

enum Mode {
    Default,
    Insert,
}

struct Sheet {
    data: CellVec,

    base_col_margin: u32,
    base_row_margin: u32,

    col_widths: Vec<u32>,
    row_heights: Vec<u32>,

    cursor: Coord,
    mode: Mode,
    temp_buffer: String,
}

fn is_a_num(token: &Option<Token>) -> bool {
    token
        .as_ref()
        .is_some_and(|t| matches!(t, &Token::Number(_)))
}

impl Sheet {
    fn new(rows: usize, cols: usize) -> Self {
        Sheet {
            data: CellVec::new(rows, cols),
            col_widths: vec![12; cols],
            row_heights: vec![2; rows],
            cursor: Coord(0, 0),
            base_col_margin: 5,
            base_row_margin: 1,
            mode: Mode::Default,
            temp_buffer: String::new(),
        }
    }

    fn parse_expression_to_token(&mut self, cell: Coord, expr: String) -> Token {
        let tokens = expr
            .strip_prefix('=')
            .unwrap()
            .split_inclusive(SPLITTABLE_TOKENS_SPACE)
            .map(|s| s.trim())
            .map(|s| {
                if let Some(idx) = s.find(SPLITTABLE_TOKENS) {
                    let (a, b) = s.split_at(idx);
                    vec![a, b]
                } else {
                    vec![s]
                }
            })
            .flatten()
            .filter(|s| s.len() != 0)
            .map(|token| {
                if CELL_REGEX.is_match(token) {
                    let coord = letters_to_coord(token);

                    if self.data.get_all_dependents(cell, vec![]).contains(&coord) {
                        return Token::Error(ERROR_SELF_REFERENTIAL);
                    }

                    self.data[token].referenced_by.insert(cell);

                    Token::Reference(coord, self.data[coord].value.clone())
                } else {
                    if let Ok(num) = token.parse::<f64>() {
                        Token::Number(num)
                    } else {
                        match token {
                            "+" => Token::Add,
                            "-" => Token::Sub,
                            "*" => Token::Mul,
                            "/" => Token::Div,
                            "%" => Token::Mod,
                            "^" => Token::Pow,
                            _ => Token::Text(token.to_owned()),
                        }
                    }
                }
            })
            .collect::<Vec<_>>();

        let mut stack: Vec<Token> = Vec::new();

        for token in tokens.into_iter() {
            match token {
                Token::Text(_) | Token::Number(_) => stack.push(token),
                Token::Reference(coord, value) => {
                    match value {
                        CellValue::Empty => stack.push(Token::Number(0.0)), // maybe strange default behaviour
                        CellValue::Numerical(num) => stack.push(Token::Number(num)),
                        CellValue::Text(text) => stack.push(Token::Text(text)),
                        CellValue::Expression(expr) => {
                            stack.push(self.parse_expression_to_token(coord, expr))
                        }
                    }
                }
                Token::Error(_) => {
                    return token;
                }
                Token::Add => {
                    let maybe_a = stack.pop();
                    let maybe_b = stack.pop();
                    if is_a_num(&maybe_a) && is_a_num(&maybe_b) {
                        // these if statements are useless because it's already guaranteed
                        // that the tokens are numbers
                        if let Token::Number(a) = maybe_a.unwrap() {
                            if let Token::Number(b) = maybe_b.unwrap() {
                                stack.push(Token::Number(a + b));
                            }
                        }
                    }
                }
                Token::Sub => {
                    let maybe_a = stack.pop();
                    let maybe_b = stack.pop();
                    if is_a_num(&maybe_a) && is_a_num(&maybe_b) {
                        // these if statements are useless because it's already guaranteed
                        // that the tokens are numbers
                        if let Token::Number(a) = maybe_a.unwrap() {
                            if let Token::Number(b) = maybe_b.unwrap() {
                                stack.push(Token::Number(b - a));
                            }
                        }
                    }
                }
                Token::Mul => {
                    let maybe_a = stack.pop();
                    let maybe_b = stack.pop();
                    if is_a_num(&maybe_a) && is_a_num(&maybe_b) {
                        // these if statements are useless because it's already guaranteed
                        // that the tokens are numbers
                        if let Token::Number(a) = maybe_a.unwrap() {
                            if let Token::Number(b) = maybe_b.unwrap() {
                                stack.push(Token::Number(a * b));
                            }
                        }
                    }
                }
                Token::Div => {
                    let maybe_a = stack.pop();
                    let maybe_b = stack.pop();
                    if is_a_num(&maybe_a) && is_a_num(&maybe_b) {
                        // these if statements are useless because it's already guaranteed
                        // that the tokens are numbers
                        if let Token::Number(a) = maybe_a.unwrap() {
                            if let Token::Number(b) = maybe_b.unwrap() {
                                if a == 0.0 {
                                    return Token::Error(ERROR_DIVISION_BY_ZERO);
                                }
                                stack.push(Token::Number(b / a));
                            }
                        }
                    }
                }
                Token::Pow => {
                    let maybe_a = stack.pop();
                    let maybe_b = stack.pop();
                    if is_a_num(&maybe_a) && is_a_num(&maybe_b) {
                        // these if statements are useless because it's already guaranteed
                        // that the tokens are numbers
                        if let Token::Number(a) = maybe_a.unwrap() {
                            if let Token::Number(b) = maybe_b.unwrap() {
                                stack.push(Token::Number(a.powf(b)));
                            }
                        }
                    }
                }
                Token::Mod => {
                    let maybe_a = stack.pop();
                    let maybe_b = stack.pop();
                    if is_a_num(&maybe_a) && is_a_num(&maybe_b) {
                        // these if statements are useless because it's already guaranteed
                        // that the tokens are numbers
                        if let Token::Number(a) = maybe_a.unwrap() {
                            if let Token::Number(b) = maybe_b.unwrap() {
                                stack.push(Token::Number(a % b));
                            }
                        }
                    }
                }
            }
        }

        if let Some(last_token) = stack.pop() {
            last_token
        } else {
            Token::Error(ERROR_NO_VALUE_LEFT_ON_STACK)
        }
    }

    fn draw_cell(&mut self, win: &mut Curses, cell: Coord) -> CursesRes {
        let initial_x_margin =
            self.base_col_margin + self.col_widths.iter().take(cell.0).sum::<u32>();
        let initial_y_margin =
            self.base_row_margin + self.row_heights.iter().take(cell.1).sum::<u32>();

        win.move_cursor(Position {
            x: initial_x_margin,
            y: initial_y_margin,
        })?;

        // top corner of cell should be ┌ if (0, 0); ├ if (0, y), ┬ if (x, 0) and ┼ otherwise
        let top_left_corner = match cell {
            Coord(0, 0) => win.acs_ulcorner(),
            Coord(0, _) => win.acs_ltee(),
            Coord(_, 0) => win.acs_ttee(),
            _ => win.acs_plus(),
        };

        win.print_ch(top_left_corner)?;

        for _ in 1..self.col_widths[cell.0] {
            win.print_ch(win.acs_hline())?;
        }

        win.move_cursor(Position {
            x: initial_x_margin,
            y: initial_y_margin + 1,
        })?;

        for _ in 1..self.row_heights[cell.1] {
            win.print_ch(win.acs_vline())?;

            let cur_pos = win.get_cursor_position();
            win.move_cursor(Position {
                y: cur_pos.y + 1,
                x: cur_pos.x - 1,
            })?;
        }

        win.move_cursor(Position {
            x: initial_x_margin + 1,
            y: initial_y_margin + 1,
        })?;

        let string_rep_of_content = format!(
            "{:width$.width$}",
            match &self.data[cell].value {
                CellValue::Empty => String::from(""),
                CellValue::Numerical(num) => format!(
                    "{:.precision$}",
                    num,
                    precision = self.data[cell].precision as usize
                ),
                CellValue::Text(txt) => txt.clone(),
                CellValue::Expression(expr) =>
                    match self.parse_expression_to_token(cell, expr.clone()) {
                        Token::Number(num) => format!(
                            "{:.precision$}",
                            num,
                            precision = self.data[cell].precision as usize
                        ),
                        Token::Text(text) => text,
                        Token::Error(err) => err.to_owned(),
                        _ => ERROR_RESIDUAL_OPERATOR.to_owned(),
                    },
            },
            width = self.col_widths[cell.0] as usize - 1
        );

        for c in string_rep_of_content.chars() {
            if self.cursor == cell {
                // current cell is the selected one
                win.set_active_color_pair(Some(LINEN_ACTIVE_PAIR))?;
            }
            win.print_ch(c)?;
        }

        win.set_active_color_pair(None)?;

        // draw edges
        let on_right_edge = cell.0 == self.data.cols - 1;
        let on_bottom_edge = cell.1 == self.data.rows - 1;

        if on_right_edge {
            win.move_cursor(Position {
                x: initial_x_margin + self.col_widths[cell.0],
                y: initial_y_margin,
            })?;
            for i in 0..self.row_heights[cell.1] {
                win.print_ch(if i == 0 {
                    if cell.1 == 0 {
                        win.acs_urcorner()
                    } else {
                        win.acs_rtee()
                    }
                } else {
                    win.acs_vline()
                })?;

                let cur_pos = win.get_cursor_position();
                win.move_cursor(Position {
                    y: cur_pos.y + 1,
                    x: cur_pos.x - 1,
                })?;
            }
        }

        if on_bottom_edge {
            win.move_cursor(Position {
                x: initial_x_margin,
                y: initial_y_margin + self.row_heights[cell.1],
            })?;

            for i in 0..self.col_widths[cell.0] {
                win.print_ch(if i == 0 {
                    if cell.0 == 0 {
                        win.acs_llcorner()
                    } else {
                        win.acs_btee()
                    }
                } else {
                    win.acs_hline()
                })?;
            }
        }

        Ok(())
    }

    fn print_cell_name(&self, win: &mut Curses) -> CursesRes {
        win.move_cursor(Position { x: 0, y: 0 })?;
        // TODO: pad this, so that when going from a bigger to a smaller cell a space is drawn over the previous bracket
        win.print_str(&format!(
            "{}{}",
            col_to_str(self.cursor.0),
            self.cursor.1 + 1
        ))?;
        Ok(())
    }

    fn print_full(&mut self, win: &mut Curses) -> CursesRes {
        win.clear()?;

        self.print_cell_name(win)?;

        // drawing the row names
        {
            let mut y = self.base_row_margin + 1;
            let mut i = 0;
            while i < self.data.rows {
                win.move_cursor(Position { x: 0, y })?;

                win.print_str(&(i + 1).to_string())?;

                y += self.row_heights[i];
                i += 1;
            }
        }

        // drawing the column names
        {
            let mut x = self.base_col_margin + 1;
            let mut i = 0;
            while i < self.data.cols {
                win.move_cursor(Position { x, y: 0 })?;

                win.print_str(&col_to_str(i))?;

                x += self.col_widths[i];
                i += 1;
            }
        }

        for y in 0..self.data.rows {
            for x in 0..self.data.cols {
                self.draw_cell(win, Coord(x, y))?;
            }
        }

        // last little lower right corner piece
        let max_x = self.base_col_margin + self.col_widths.iter().sum::<u32>();
        let max_y = self.base_row_margin + self.row_heights.iter().sum::<u32>();
        win.move_cursor(Position { x: max_x, y: max_y })?;
        win.insert_ch(win.acs_lrcorner())?;

        Ok(())
    }

    fn cursor_move(&mut self, win: &mut Curses, dir: Direction) -> CursesRes {
        let old_pos = self.cursor;
        match dir {
            Direction::Left => {
                self.cursor.0 = self.cursor.0.saturating_sub(1);
            }
            Direction::Right => {
                self.cursor.0 = (self.cursor.0 + 1).min(self.data.cols - 1);
            }
            Direction::Up => {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
            }
            Direction::Down => {
                self.cursor.1 = (self.cursor.1 + 1).min(self.data.rows - 1);
            }
        }
        self.draw_cell(win, old_pos)?;
        self.draw_cell(win, self.cursor)?;

        self.print_cell_name(win)?;

        Ok(())
    }

    fn switch_mode(&mut self, win: &mut Curses, new_mode: Mode, set_buffer: bool) -> CursesRes {
        match new_mode {
            Mode::Default => {
                self.mode = Mode::Default;
                self.clear_input_bar(win)?;
            }
            Mode::Insert => {
                self.mode = Mode::Insert;
                if set_buffer {
                    self.temp_buffer = self.data[self.cursor].value.to_string();
                }
                self.draw_input_bar(win)?;
            }
        }

        Ok(())
    }

    fn draw_input_bar(&self, win: &mut Curses) -> CursesRes {
        let size = win.get_terminal_size();
        win.move_cursor(Position {
            x: 0,
            y: size.y_count - 3,
        })?;
        win.print_ch(win.acs_ulcorner())?;
        for _ in 1..size.x_count - 1 {
            win.print_ch(win.acs_hline())?;
        }
        win.print_ch(win.acs_urcorner())?; // this will wrap the cursor to the next line
        win.print_ch(win.acs_vline())?; // so this is already correctly positioned
        win.print_ch(' ')?; // easier than moving the cursor manually

        if !self.temp_buffer.is_empty() {
            // TODO: truncate this if it happens to be longer than the screen can fit
            win.print_str(&self.temp_buffer)?; // actual buffer content
        }
        win.print_ch('|')?;
        win.print_ch(' ')?; // to ensure the backspace visuals will be cleared up

        win.move_cursor(Position {
            x: size.x_count - 1,
            y: size.y_count - 2,
        })?;
        win.print_ch(win.acs_vline())?; // this also wraps
        win.print_ch(win.acs_llcorner())?;
        for _ in 1..size.x_count - 1 {
            win.print_ch(win.acs_hline())?;
        }
        win.insert_ch(win.acs_lrcorner())?;

        Ok(())
    }

    fn clear_input_bar(&self, win: &mut Curses) -> CursesRes {
        let size = win.get_terminal_size();
        win.move_cursor(Position {
            x: 0,
            y: size.y_count - 3,
        })?;

        for _ in 0..(size.x_count * 3 - 1) {
            win.print_ch(' ')?;
        }
        win.insert_ch(' ')?;

        Ok(())
    }

    fn parse_input(&mut self, win: &mut Curses, cell: Coord) -> CursesRes {
        let input = self.temp_buffer.clone();

        if let Ok(num) = input.parse::<f64>() {
            self.data[cell].value = CellValue::Numerical(num);
        } else if input.starts_with('=') {
            self.data[cell].value = CellValue::Expression(input);
        } else if input.is_empty() {
            self.data[cell].value = CellValue::Empty;
        } else {
            self.data[cell].value = CellValue::Text(input);
        }

        self.draw_cell(win, cell)?;

        for dependent in self.data.get_all_dependents(cell, vec![]) {
            self.draw_cell(win, dependent)?;
        }

        Ok(())
    }

    fn discard(&mut self, win: &mut Curses) -> CursesRes {
        self.temp_buffer.clear();
        self.clear_input_bar(win)?;
        self.mode = Mode::Default;

        Ok(())
    }
}

fn col_to_str(col: usize) -> String {
    assert!(col < 25);
    String::from((col + 65) as u8 as char)
}

fn main() -> CursesRes {
    let mut sheet = Sheet::new(10, 10);

    let mut win = Curses::init();

    win.set_echo(false)?;
    win.set_cursor_visibility(CursorVisibility::Invisible)?;

    if win.can_change_colors() {
        win.set_color_id_rgb(LINEN_BG_ACTIVE, [0.3, 0.3, 0.3])?;
        win.set_color_pair_content(LINEN_ACTIVE_PAIR, ColorID::WHITE, LINEN_BG_ACTIVE)?;
    }

    sheet.print_full(&mut win)?;

    'run: while let Some(ev) = win.poll_events() {
        match sheet.mode {
            Mode::Default => {
                match ev {
                    CursesKey::Ascii(b'q') | CursesKey::Ascii(0x1b) => break 'run,
                    CursesKey::Ascii(b'h') => {
                        sheet.col_widths[sheet.cursor.0] =
                            (sheet.col_widths[sheet.cursor.0] - 1).max(2);
                        sheet.print_full(&mut win)?;
                    } // narrower column
                    CursesKey::Ascii(b'j') => {
                        sheet.row_heights[sheet.cursor.1] =
                            (sheet.row_heights[sheet.cursor.1] - 1).max(2);
                        sheet.print_full(&mut win)?;
                    } // smaller  row
                    CursesKey::Ascii(b'k') => {
                        sheet.row_heights[sheet.cursor.1] = sheet.row_heights[sheet.cursor.1] + 1;
                        sheet.print_full(&mut win)?;
                    } // taller   row
                    CursesKey::Ascii(b'l') => {
                        sheet.col_widths[sheet.cursor.0] = sheet.col_widths[sheet.cursor.0] + 1;
                        sheet.print_full(&mut win)?;
                    } // wider    column
                    CursesKey::Ascii(b'i') | CursesKey::Ascii(b'\n') | CursesKey::Insert => {
                        sheet.switch_mode(&mut win, Mode::Insert, true)?;
                    }
                    CursesKey::Ascii(b'u') => {
                        sheet.print_full(&mut win)?;
                    }
                    CursesKey::Ascii(b'p') => {
                        sheet.data[sheet.cursor].precision =
                            sheet.data[sheet.cursor].precision.saturating_add(1);
                        sheet.draw_cell(&mut win, sheet.cursor)?;
                    }
                    CursesKey::Ascii(b'o') => {
                        sheet.data[sheet.cursor].precision =
                            sheet.data[sheet.cursor].precision.saturating_sub(1);
                        sheet.draw_cell(&mut win, sheet.cursor)?;
                    }
                    CursesKey::Ascii(b'=') => {
                        sheet.temp_buffer = String::from("=");
                        sheet.switch_mode(&mut win, Mode::Insert, false)?;
                    }
                    CursesKey::ArrowLeft => {
                        sheet.cursor_move(&mut win, Direction::Left)?;
                    }
                    CursesKey::ArrowRight => {
                        sheet.cursor_move(&mut win, Direction::Right)?;
                    }
                    CursesKey::ArrowUp => {
                        sheet.cursor_move(&mut win, Direction::Up)?;
                    }
                    CursesKey::ArrowDown => {
                        sheet.cursor_move(&mut win, Direction::Down)?;
                    }
                    _ => {}
                }
            }
            Mode::Insert => match ev {
                CursesKey::Insert | CursesKey::Ascii(b'\n') => {
                    sheet.parse_input(&mut win, sheet.cursor)?;
                    sheet.switch_mode(&mut win, Mode::Default, true)?;
                }
                CursesKey::Ascii(0x1b) => {
                    sheet.discard(&mut win)?;
                }
                CursesKey::Ascii(key) => {
                    sheet.temp_buffer.push(key as char);
                    sheet.draw_input_bar(&mut win)?;
                }
                CursesKey::Backspace => {
                    sheet.temp_buffer.pop();
                    sheet.draw_input_bar(&mut win)?;
                }
                _ => {}
            },
        }
    }

    drop(win);

    println!("cursor was on {:?}", sheet.data[sheet.cursor]);

    Ok(())
}
