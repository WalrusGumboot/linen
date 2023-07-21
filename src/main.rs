#![allow(dead_code)]

use std::{ops::{Index, IndexMut}, num::NonZeroU8};

use yacurses::*;

static LINEN_RED: ColorID = ColorID(0x11);
static LINEN_BG_ACTIVE: ColorID = ColorID(0x12);
static LINEN_ACTIVE_PAIR: ColorPair = ColorPair(unsafe { NonZeroU8::new_unchecked(255) }); // SAFETY: trivial

type CursesRes = Result<(), Box<dyn std::error::Error>>;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
struct Coord(usize, usize); // (x, y)

#[derive(Clone)]
enum CellValue {
    Numerical(f64),     // a constant number
    Text(String),       // a constant string
    Expression(String), // a "formula"
    Empty
}

use CellValue::*;

#[derive(Clone)]
struct SheetCell {
    coord: Coord,
    value: CellValue,
    precision: u8
}

impl SheetCell {
    fn new(coord: Coord) -> Self {
        SheetCell { coord, value: CellValue::Empty, precision: 0 }
    }
}

struct CellVec {
    rows: usize,
    cols: usize,
    data: Vec<SheetCell>
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

impl CellVec {
    fn new(rows: usize, cols: usize) -> Self {
        CellVec { 
            rows, 
            cols, 
            data: (0..rows).flat_map(move |x| std::iter::repeat(x).zip(0..cols)).map(|(r, c)| SheetCell::new(Coord(r, c))).collect::<Vec<_>>() 
        }
    }
}

enum Direction {
    Left,
    Right,
    Up,
    Down
}

enum Mode {
    Default,
    Insert
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

impl Sheet {
    fn new(rows: usize, cols: usize) -> Self {
        Sheet { 
            data: CellVec::new(rows, cols), 
            col_widths: vec![12, 12, 22, 12, 12, 12, 12, 12, 12, 12], 
            row_heights: vec![2,2,5,2,2,2,2,2,2,2], 
            cursor: Coord(0, 0), 
            base_col_margin: 5,
            base_row_margin: 1,
            mode: Mode::Default,
            temp_buffer: String::from("test"),
        }
    }

    fn draw_cell(&self, win: &mut Curses, cell: Coord) -> CursesRes {
        let initial_x_margin = self.base_col_margin + self.col_widths.iter().take(cell.0).sum::<u32>();
        let initial_y_margin = self.base_row_margin + self.row_heights.iter().take(cell.1).sum::<u32>();

        win.move_cursor(Position { x: initial_x_margin, y: initial_y_margin })?;
        
        // top corner of cell should be ┌ if (0, 0); ├ if (0, y), ┬ if (x, 0) and ┼ otherwise
        let top_left_corner = match cell {
            Coord(0, 0) => win.acs_ulcorner(),
            Coord(0, _) => win.acs_ltee(),
            Coord(_, 0) => win.acs_ttee(),
            _           => win.acs_plus()
        };
        
        win.print_ch(top_left_corner)?;
        
        for _ in 1..self.col_widths[cell.0] {
            win.print_ch(win.acs_hline())?;
        }
        
        win.move_cursor(Position { x: initial_x_margin, y: initial_y_margin + 1})?;

        for _ in 1..self.row_heights[cell.1] {
            win.print_ch(win.acs_vline())?;

            let cur_pos = win.get_cursor_position();
            win.move_cursor(Position { y: cur_pos.y + 1, x: cur_pos.x - 1})?;
        }

        win.move_cursor(Position { x: initial_x_margin + 1, y: initial_y_margin + 1})?;

        let content = &self.data[cell];

        let string_rep_of_content = match content.value.clone() {
            Empty => format!("({}, {})", cell.0, cell.1),
            Numerical(num) => num.to_string(),
            Text(val) => val.to_string(),
            Expression(_) => String::from("todo"),
        };

        for c in string_rep_of_content.chars() {
            if self.cursor == cell {
                // current cell is the selected one
                win.set_active_color_pair(Some(LINEN_ACTIVE_PAIR))?;                
            }
            win.print_ch(c)?;
        }

        win.set_active_color_pair(None)?;

        // draw edges
        let on_right_edge  = cell.0 == self.data.cols - 1;
        let on_bottom_edge = cell.1 == self.data.rows - 1;
        
        if on_right_edge {
            win.move_cursor(Position { x: initial_x_margin + self.col_widths[cell.0], y: initial_y_margin })?;
            for i in 0..self.row_heights[cell.1] {
                win.print_ch(
                    if i == 0 { 
                        if cell.1 == 0 {
                            win.acs_urcorner() 
                        } else {
                            win.acs_rtee() 
                        }
                    } else {             
                        win.acs_vline() 
                    }
                )?;

                let cur_pos = win.get_cursor_position();
                win.move_cursor(Position { y: cur_pos.y + 1, x: cur_pos.x - 1})?;
            }
        }

        if on_bottom_edge {
            win.move_cursor(Position { x: initial_x_margin, y: initial_y_margin + self.row_heights[cell.1] })?;

            for i in 0..self.col_widths[cell.0] {
                win.print_ch(
                    if i == 0 {
                        if cell.0 == 0 {
                            win.acs_llcorner()
                        } else {
                            win.acs_btee()
                        }
                    } else {
                        win.acs_hline()
                    }
                )?;
            }
        }

       

        Ok(())
    }

    fn print_cell_name(&self, win: &mut Curses) -> CursesRes {
        win.move_cursor(Position { x: 0, y: 0 })?;
        // TODO: pad this, so that when going from a bigger to a smaller cell a space is drawn over the previous bracket
        win.print_str(&format!("[{}{}]", col_to_str(self.cursor.0), self.cursor.1 + 1))?; 
        Ok(())
    }

    fn print_full(&self, win: &mut Curses) -> CursesRes {
        win.clear()?;

        self.print_cell_name(win)?;
        
        // drawing the row names
        {
            let mut y = self.base_row_margin + (self.base_row_margin as f64 / 2.0).ceil() as u32;
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
            let mut x = self.base_col_margin + (self.base_col_margin as f64 / 2.0).ceil() as u32;
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

        // win.print_str(&format!("max x: {max_x}, max y: {max_y}"))?;

        win.move_cursor(Position { x: max_x, y: max_y })?;

        win.insert_ch(win.acs_lrcorner())?;
        
        
        Ok(())
    }

    fn cursor_move(&mut self, win: &mut Curses, dir: Direction) -> CursesRes {
        let old_pos = self.cursor;
        match dir {
            Direction::Left => { self.cursor.0 = self.cursor.0.saturating_sub(1); }
            Direction::Right => { self.cursor.0 = (self.cursor.0 + 1).min(self.data.cols - 1); },
            Direction::Up => { self.cursor.1 = self.cursor.1.saturating_sub(1); },
            Direction::Down => { self.cursor.1 = (self.cursor.1 + 1).min(self.data.rows - 1); }
        }
        self.draw_cell(win, old_pos)?;
        self.draw_cell(win, self.cursor)?;

        self.print_cell_name(win)?;

        Ok(())
    }

    fn switch_mode(&mut self, win: &mut Curses, new_mode: Mode) -> CursesRes {
        match new_mode {
            Mode::Default => {
                self.mode = Mode::Default;
                self.clear_input_bar(win)?;
            },
            Mode::Insert => {
                self.mode = Mode::Insert;
                self.draw_input_bar(win)?;
            }
        }

        Ok(())
    }

    fn draw_input_bar(&self, win: &mut Curses) -> CursesRes {
        let size = win.get_terminal_size();
        win.move_cursor(Position { x: 0, y: size.y_count - 3 })?;
        win.print_ch(win.acs_ulcorner())?;
        for _ in 1..size.x_count-1 {
            win.print_ch(win.acs_hline())?;
        }
        win.print_ch(win.acs_urcorner())?; // this will wrap the cursor to the next line
        win.print_ch(win.acs_vline())?; // so this is already correctly positioned
        win.print_ch(' ')?; // easier than moving the cursor manually
        win.print_str(&self.temp_buffer)?; // actual buffer content
        win.move_cursor(Position { x: size.x_count - 1, y: size.y_count - 2 })?;
        win.print_ch(win.acs_vline())?; // this also wraps
        win.print_ch(win.acs_llcorner())?;
        for _ in 1..size.x_count-1 {
            win.print_ch(win.acs_hline())?;
        }
        win.insert_ch(win.acs_lrcorner())?;
        
        Ok(())
    }

    fn clear_input_bar(&self, win: &mut Curses) -> CursesRes {
        let size = win.get_terminal_size();
        win.move_cursor(Position { x: 0, y: size.y_count - 3 })?;

        for _ in 0..(size.x_count * 3 - 1) {
            win.print_ch(' ')?;
        }
        win.insert_ch(' ')?;

        Ok(())
    }

    fn parse_input(&mut self, _cell: Coord) {
        //todo!();
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

    win.set_color_id_rgb(LINEN_BG_ACTIVE, [0.3, 0.3, 0.3])?;
    win.set_color_pair_content(LINEN_ACTIVE_PAIR, ColorID::WHITE, LINEN_BG_ACTIVE)?;

    sheet.print_full(&mut win)?;

    'run: while let Some(ev) = win.poll_events() {
        match sheet.mode {
            Mode::Default => {
                match ev {
                    CursesKey::Ascii(b'q') => break 'run,
                    CursesKey::Insert =>     { sheet.switch_mode(&mut win, Mode::Insert)?; }
                    CursesKey::ArrowLeft =>  { sheet.cursor_move(&mut win, Direction::Left)?; },
                    CursesKey::ArrowRight => { sheet.cursor_move(&mut win, Direction::Right)?; },
                    CursesKey::ArrowUp =>    { sheet.cursor_move(&mut win, Direction::Up)?; },
                    CursesKey::ArrowDown =>  { sheet.cursor_move(&mut win, Direction::Down)?; },
                    _ => {}
                }
            },
            Mode::Insert => {
                match ev {
                    CursesKey::Ascii(key) => { sheet.temp_buffer.push(key as char); },
                    CursesKey::Backspace => { sheet.temp_buffer.pop(); }
                    CursesKey::Insert => { sheet.parse_input(sheet.cursor); sheet.switch_mode(&mut win, Mode::Default)?; },
                    _ => {  }
                }
            }  
        }
    }
    

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn col_to_string() {
        assert_eq!(col_to_str(0), String::from("A"));
        assert_eq!(col_to_str(1), String::from("B"));
        assert_eq!(col_to_str(25), String::from("Z"));
        assert_eq!(col_to_str(26), String::from("AA"));
        assert_eq!(col_to_str(51), String::from("AZ"));
        assert_eq!(col_to_str(52), String::from("BA"));
        assert_eq!(col_to_str(675), String::from("ZZ"));
        assert_eq!(col_to_str(676), String::from("AAA"));
    }
}
