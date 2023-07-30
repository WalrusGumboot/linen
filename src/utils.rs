use crate::sheet::Token;

pub fn col_to_str(col: usize) -> String {
    assert!(col < 25);
    String::from((col + 65) as u8 as char)
}

pub fn is_a_num(token: &Option<Token>) -> bool {
    token
        .as_ref()
        .is_some_and(|t| matches!(t, &Token::Number(_)))
}

pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

pub type CursesRes = Result<(), Box<dyn std::error::Error>>;