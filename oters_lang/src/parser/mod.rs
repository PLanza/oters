pub mod ast;
mod errors;
pub mod span;
mod tests;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub oters);

use self::ast::Program;
use crate::errors::SpError;

pub fn parse_source(source: String) -> Result<Program, SpError> {
    let parser = oters::ProgramParser::new();

    // The parsed abstract syntax tree
    let result = parser.parse(&source);

    match result {
        Ok(ast) => Ok(ast),
        Err(e) => match e {
            lalrpop_util::ParseError::ExtraToken { token } => Err(SpError::new(
                errors::ParseError::ExtraTokenError.into(),
                (token.0, token.2),
            )),
            lalrpop_util::ParseError::InvalidToken { location } => Err(SpError::new(
                errors::ParseError::InvalidTokenError.into(),
                (location, location + 1),
            )),
            lalrpop_util::ParseError::UnrecognizedToken { token, .. } => Err(SpError::new(
                errors::ParseError::InvalidTokenError.into(),
                (token.0, token.2),
            )),
            lalrpop_util::ParseError::UnrecognizedEOF { location, .. } => Err(SpError::new(
                errors::ParseError::UnrecognizedEOFError.into(),
                (location - 1, location),
            )),
            lalrpop_util::ParseError::User { .. } => unreachable!(),
        },
    }
}
