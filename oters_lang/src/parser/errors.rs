use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Extra token")]
    ExtraTokenError,
    #[error("Invalid token")]
    InvalidTokenError,
    #[error("Unrecognized token")]
    UnrecognizedTokenError,
    #[error("Unexpected end of file")]
    UnrecognizedEOFError,
}
