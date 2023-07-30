use std::{collections::HashSet, ops::{Index, IndexMut}};
use thiserror::Error;

use crate::utils::col_to_str;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub struct Coord(pub usize, pub usize); // (x, y)

impl std::fmt::Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", col_to_str(self.0), self.1 + 1)
    }
}


#[derive(Clone, Debug)]
pub enum CellValue {
    Numerical(f64),     // a constant number
    Text(String),       // a constant string
    Expression(String), // a "formula"
    Empty,
}

impl From<String> for CellValue {
    fn from(value: String) -> Self {
        if let Ok(num) = value.parse::<f64>() {
            CellValue::Numerical(num)
        } else if value.starts_with('=') {
            CellValue::Expression(value)
        } else if value.is_empty() {
            CellValue::Empty
        } else {
            CellValue::Text(value)
        }
    }
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
pub struct SheetCell {
    pub coord: Coord,
    pub value: CellValue,
    pub precision: u8,
    pub referenced_by: HashSet<Coord>,
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
pub struct CellVec {
    pub rows: usize,
    pub cols: usize,
    pub data: Vec<SheetCell>,
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

#[derive(Debug, Error)]
pub enum CoordinateConversionError {
    #[error("Could not parse column data from {0}.")]
    ColumnParseError(String),
    #[error("Could not parse row data from {0}.")]
    RowParseError(String),
}

impl TryFrom<&str> for Coord {
    type Error = CoordinateConversionError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let letters = value
            .chars()
            .filter(|c| c.is_alphabetic())
            .collect::<String>();
        let maybe_numbers = value
            .chars()
            .filter(|c| c.is_numeric())
            .collect::<String>()
            .parse::<usize>();

        if letters.is_empty() {
            return Err(CoordinateConversionError::ColumnParseError(
                value.to_owned(),
            ));
        }

        if let Ok(numbers) = maybe_numbers {
            // TODO: support letter parts bigger than one letter
            let col = letters.chars().next().unwrap() as usize - 65;
            let row = numbers - 1;

            Ok(Coord(col, row))
        } else {
            Err(CoordinateConversionError::RowParseError(value.to_owned()))
        }
    }
}

impl Index<&str> for CellVec {
    type Output = SheetCell;
    fn index(&self, index: &str) -> &Self::Output {
        &self[Coord::try_from(index).unwrap()]
    }
}

impl IndexMut<&str> for CellVec {
    fn index_mut(&mut self, index: &str) -> &mut Self::Output {
        &mut self[Coord::try_from(index).unwrap()]
    }
}

impl CellVec {
    pub fn new(rows: usize, cols: usize) -> Self {
        CellVec {
            rows,
            cols,
            data: (0..rows)
                .flat_map(move |x| std::iter::repeat(x).zip(0..cols))
                .map(|(r, c)| SheetCell::new(Coord(r, c)))
                .collect::<Vec<_>>(),
        }
    }

    pub fn get_all_dependents(&self, cell: Coord, path: Vec<Coord>) -> Vec<Coord> {
        let dependents = self[cell]
            .referenced_by
            .iter()
            .filter(|d| !path.contains(d))
            .collect::<Vec<_>>();
        if dependents.is_empty() {
            vec![cell]
        } else {
            dependents
                .iter()
                .flat_map(|d| {
                    let mut new_path = path.clone();
                    new_path.push(cell);
                    self.get_all_dependents(**d, new_path)
                })
                .collect()
        }
    }
}