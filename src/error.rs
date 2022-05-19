use std::{
    fmt::{Display, Formatter},
    io,
};

#[derive(Debug)]
pub enum MError {
    RustError(io::Error),
    ParseError(String),
}

impl Display for MError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match *self {
            MError::ParseError(ref s) => write!(f, "parse error: {}", s),
            MError::RustError(ref e) => write!(f, "rust error: {}", e),
        };
        Ok(())
    }
}

impl std::convert::From<io::Error> for MError {
    fn from(e: io::Error) -> Self {
        MError::RustError(e)
    }
}
