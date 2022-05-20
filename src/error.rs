use std::{
    fmt::{Display, Formatter},
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
