use std::{
    io,
};

use derive_more::Display;

#[derive(Debug, Display)]
pub enum MError {
    #[display(fmt = "Rust Error: {}", _0)]
    RustError(io::Error),
    #[display(fmt = "Parser Error: {}", _0)]
    ParseError(String),
}

impl std::convert::From<io::Error> for MError {
    fn from(e: io::Error) -> Self {
        MError::RustError(e)
    }
}

pub fn print_parser_errors(errors: &Vec<MError>) {
    for (i, error) in errors.iter().enumerate() {
        println!("{i}@{error}");
    }
}
