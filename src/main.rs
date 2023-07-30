use std::num::NonZeroU8;

use lazy_static::lazy_static;
use regex::Regex;

use yacurses::*;

mod utils;
use utils::*;

mod cells;
use cells::*;

mod sheet;
use sheet::*;

static ERROR_SELF_REFERENTIAL: &str = "#SELFREF";
static ERROR_NO_VALUE_LEFT_ON_STACK: &str = "#NO_VALUE";
static ERROR_RESIDUAL_OPERATOR: &str = "#RESID_OP";
static ERROR_DIVISION_BY_ZERO: &str = "#DIV_BY_0";

static SPLITTABLE_TOKENS: &[char] = &['+', '-', '*', '/', '%', '$', '&', '^'];
static SPLITTABLE_TOKENS_SPACE: &[char] = &['+', '-', '*', '/', '%', '$', '&', '^', ' '];

static LINEN_BG_ACTIVE: ColorID = ColorID(0x12);
static LINEN_ACTIVE_PAIR: ColorPair = ColorPair(unsafe { NonZeroU8::new_unchecked(255) }); // SAFETY: trivial

lazy_static! {
    static ref CELL_REGEX: Regex = Regex::new("[A-Z]+[1-9]+[0-9]*").unwrap();
}

fn main() -> CursesRes {
    let mut sheet = if let Some(path) = std::env::args().nth(1) {
        Sheet::from_file(path) // slightly janky but oh well
    } else {
        Sheet::new(10, 10)
    };

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
                    CursesKey::Ascii(b's') => {
                        sheet.save(&mut win)?;
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

    //println!("cursor was on {:?}", sheet.data[sheet.cursor]);

    Ok(())
}
