use std::{path::{PathBuf, Path}, collections::HashSet, time::Duration};
use proctitle::set_title;
use yacurses::{Curses, Position};

use crate::{CellVec, Coord, cells::{CellValue, SheetCell}, SPLITTABLE_TOKENS_SPACE, SPLITTABLE_TOKENS, CELL_REGEX, ERROR_SELF_REFERENTIAL, utils::{is_a_num, col_to_str, Direction}, ERROR_DIVISION_BY_ZERO, ERROR_NO_VALUE_LEFT_ON_STACK, CursesRes, ERROR_RESIDUAL_OPERATOR, LINEN_ACTIVE_PAIR};

pub enum Mode {
    Default,
    Insert,
}

#[derive(Debug)]
pub enum Token {
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


pub struct Sheet {
    pub data: CellVec,

    pub base_col_margin: u32,
    pub base_row_margin: u32,

    pub col_widths: Vec<u32>,
    pub row_heights: Vec<u32>,

    pub cursor: Coord,
    pub mode: Mode,
    pub temp_buffer: String,

    pub modal_content: String,

    pub file_path: Option<PathBuf>,

    pub top_left_visible: Coord, // used for scrolling
}

impl Sheet {
    pub fn new(rows: usize, cols: usize) -> Self {
        set_title("linen <new-buf>");
        Sheet {
            data: CellVec::new(rows, cols),
            col_widths: vec![12; cols],
            row_heights: vec![2; rows],
            cursor: Coord(0, 0),
            base_col_margin: 5,
            base_row_margin: 1,
            mode: Mode::Default,
            temp_buffer: String::new(),
            modal_content: String::new(),
            file_path: None,
            top_left_visible: Coord(0, 0)
        }
    }

    pub fn from_file<P: AsRef<Path>>(path: P) -> Self
    where
        PathBuf: From<P>,
    {
        let raw_content =
            std::fs::read_to_string(&path).expect("Could not read the provided file.");
        let mut content = raw_content.lines();

        let mut dimensions = content
            .next()
            .expect("Malformed file: missing dimension specification in first line.")
            .split('x');
        let cols = dimensions
            .next()
            .unwrap()
            .parse::<u32>()
            .expect("Malformed file: could not parse number of columns.");
        let rows = dimensions
            .next()
            .unwrap()
            .parse::<u32>()
            .expect("Malformed file: could not parse number of rows.");

        let col_widths = content
            .next()
            .expect("Malformed file: missing column width specification.")
            .split(' ')
            .map(|u| {
                u.parse::<u32>()
                    .expect("Malformed file: could not parse column width.")
            })
            .collect::<Vec<_>>();
        let row_heights = content
            .next()
            .expect("Malformed file: missing row height specification.")
            .split(' ')
            .map(|u| {
                u.parse::<u32>()
                    .expect("Malformed file: could not parse row height.")
            })
            .collect::<Vec<_>>();

        let mut data = CellVec::new(rows as usize, cols as usize);

        for line in content {
            let mut parts = line.splitn(4, ':');
            let coord_string = parts.next().unwrap();
            let coord = Coord::try_from(coord_string).unwrap();
            let precision = parts.next().unwrap().parse::<u8>().unwrap();
            let referenced_by_raw = parts.next().unwrap();
            let referenced_by = if referenced_by_raw.is_empty() {
                HashSet::new()
            } else {
                referenced_by_raw
                    .split(' ')
                    .map(|s| Coord::try_from(s).unwrap())
                    .collect::<HashSet<_>>()
            };
            let value = CellValue::from(parts.next().unwrap().to_owned());

            data[coord_string] = SheetCell {
                coord,
                precision,
                value,
                referenced_by,
            }
        }

        Sheet {
            data,
            col_widths,
            row_heights,
            base_col_margin: 5,
            base_row_margin: 1,
            mode: Mode::Default,
            cursor: Coord(0, 0),
            temp_buffer: String::new(),
            modal_content: String::new(),
            file_path: Some(path.into()),
            top_left_visible: Coord(0, 0)
        }
    }

    fn parse_expression_to_token(&mut self, cell: Coord, expr: String) -> Token {
        let tokens = expr
            .strip_prefix('=')
            .unwrap()
            .split_inclusive(SPLITTABLE_TOKENS_SPACE)
            .map(|s| s.trim())
            .flat_map(|s| {
                if let Some(idx) = s.find(SPLITTABLE_TOKENS) {
                    let (a, b) = s.split_at(idx);
                    vec![a, b]
                } else {
                    vec![s]
                }
            })
            .filter(|s| !s.is_empty())
            .map(|token| {
                if CELL_REGEX.is_match(token) {
                    let coord = Coord::try_from(token).unwrap();

                    if self.data.get_all_dependents(cell, vec![]).contains(&coord) {
                        return Token::Error(ERROR_SELF_REFERENTIAL);
                    }

                    self.data[token].referenced_by.insert(cell);

                    Token::Reference(coord, self.data[coord].value.clone())
                } else if let Ok(num) = token.parse::<f64>() {
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

    pub fn draw_cell(&mut self, win: &mut Curses, cell: Coord) -> CursesRes {
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

    pub fn print_full(&mut self, win: &mut Curses) -> CursesRes {
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

    pub fn cursor_move(&mut self, win: &mut Curses, dir: Direction) -> CursesRes {
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

    pub fn switch_mode(&mut self, win: &mut Curses, new_mode: Mode, set_buffer: bool) -> CursesRes {
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

    pub fn draw_input_bar(&self, win: &mut Curses) -> CursesRes {
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

    pub fn parse_input(&mut self, win: &mut Curses, cell: Coord) -> CursesRes {
        let input = self.temp_buffer.clone();

        self.data[cell].value = CellValue::from(input);

        self.draw_cell(win, cell)?;

        for dependent in self.data.get_all_dependents(cell, vec![]) {
            self.draw_cell(win, dependent)?;
        }

        Ok(())
    }

    pub fn discard(&mut self, win: &mut Curses) -> CursesRes {
        self.temp_buffer.clear();
        self.clear_input_bar(win)?;
        self.mode = Mode::Default;

        Ok(())
    }

    pub fn save(&mut self, win: &mut Curses) -> CursesRes {
        if let Some(path) = &self.file_path {
            let mut string_rep: Vec<String> = Vec::new();
            string_rep.push(format!("{}x{}\n", self.data.cols, self.data.rows));
            string_rep.push(
                self.col_widths
                    .iter()
                    .map(|u| u.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
                    + "\n",
            );
            string_rep.push(
                self.row_heights
                    .iter()
                    .map(|u| u.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
                    + "\n",
            );
            for cell in &self.data.data {
                if matches!(cell.value, CellValue::Empty) {
                    continue;
                }
                string_rep.push(format!(
                    "{}:{}:{}:{}\n",
                    cell.coord,
                    cell.precision,
                    cell.referenced_by
                        .iter()
                        .map(|c| format!("{}{}", col_to_str(c.0), c.1 + 1))
                        .collect::<Vec<_>>()
                        .join(" "),
                    cell.value
                ))
            }

            let glob: Vec<u8> = string_rep.iter().fold(Vec::new(), |mut acc, curr| {
                acc.append(&mut curr.as_bytes().into());
                acc
            });

            std::fs::write(path, glob.as_slice())?;

            self.modal_content = format!("Succesfully saved to {}.", path.display());
            self.draw_modal(win, Duration::from_secs(2))?;

            set_title(format!("linen {}", path.display()))
        } else {
            //TODO: display a text input dialogue
            //TODO: warn if file already exists
            self.file_path = Some(PathBuf::from("sheet.lin"));
            self.save(win)?;
        }

        Ok(())
    }

    fn draw_modal(&self, win: &mut Curses, duration: Duration) -> CursesRes {
        let size = win.get_terminal_size();
        let modal_content_length = self.modal_content.len();
        let left_edge = size.x_count / 2 - modal_content_length as u32 / 2 - 2;

        win.move_cursor(Position {
            x: left_edge,
            y: size.y_count - 3,
        })?;
        win.print_ch(win.acs_ulcorner())?;
        for _ in 0..modal_content_length + 2 {
            win.print_ch(win.acs_hline())?;
        }
        win.print_ch(win.acs_urcorner())?;
        win.move_cursor(Position {
            x: left_edge,
            y: size.y_count - 2,
        })?;
        win.print_ch(win.acs_vline())?;
        win.print_ch(' ')?;

        if !self.modal_content.is_empty() {
            // TODO: truncate this if it happens to be longer than the screen can fit, which is unlikely
            win.print_str(&self.modal_content)?;
        }

        win.print_ch(' ')?;
        win.print_ch(win.acs_vline())?;

        win.move_cursor(Position {
            x: left_edge,
            y: size.y_count - 1,
        })?;

        win.print_ch(win.acs_llcorner())?;
        for _ in 0..modal_content_length + 2 {
            win.print_ch(win.acs_hline())?;
        }
        win.insert_ch(win.acs_lrcorner())?;

        win.refresh()?;

        std::thread::sleep(duration); // blocking, sadly

        self.clear_input_bar(win)?; // we can reuse the same code

        Ok(())
    }
}