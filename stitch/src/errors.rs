//! Stitch Errors
//!

use std::error::Error;
use std::fmt;


#[derive(Clone, Debug, PartialEq)]
pub struct StitchError {
    /// Absolute character position.
    pub offset: usize,
    /// Line number in the program source.
    pub line: usize,
    /// Column number in the current line.
    pub column: usize,
    /// The line of code where the error is identified.
    pub source: String,
    pub reason: String,
}

impl StitchError {
    pub fn new(line: usize, col: usize, reason: String) -> Self {
        StitchError {
            offset: 0,
            line: line,
            column: col,
            source: String::from(""),
            reason: reason,
        }
    }
}

impl Error for StitchError {
    fn description(&self) -> &str {
        "Errors are not implemented"
    }
}

impl fmt::Display for StitchError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}:{}: {}", self.line, self.column, self.reason)
    }
}

#[cfg(test)]
mod tests {

}
