//! This module defines the error type for this program.

use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

pub enum Error {
    NoInputFile, // Error when no input file is provided
    UnsupportedFileExt, // Error when the file extension is not supported
    IO(std::io::Error), // Error when an IO operation fails
    Parse(quick_xml::de::DeError), // Error when parsing XML fails
}

impl Debug for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Error::NoInputFile => write!(f, "No input file provided. Exiting..."),
            Error::UnsupportedFileExt => write!(f, "Unsupported file extension. Supported file extensions are: .bpmn, .pnml"),
            Error::IO(e) => write!(f, "Error opening file: {}", e),
            Error::Parse(e) => write!(f, "Error parsing XML: {}", e),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Debug::fmt(self, f)
    }
}

impl std::error::Error for Error {}