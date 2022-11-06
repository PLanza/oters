mod ast;
mod tests;

use lalrpop_util::{lalrpop_mod, ParseError};
lalrpop_mod!(pub oters);

use std::fs::read_to_string;

fn get_error_message(source: &String, location: usize) -> Result<String, std::str::Utf8Error> {

    let mut line_number = 1;
    let mut line_start = 0;
    let mut line_end = 0;

    for (i, b) in source.bytes().enumerate() {
        if b == '\n' as u8 && i < location {
            line_start = i + 1;
            line_number += 1;
        }

        if b == '\n' as u8 && i >= location {
            line_end = i;
            break;
        }
    }

    let code_line = std::str::from_utf8(&source.as_bytes()[line_start..line_end])?;
    let mut under_line = (0..(location - line_start)).map(|_| " ").collect::<String>();
    under_line.push('^');

    let message = format!(
        "on line {}\n{}\n{}", line_number, code_line, under_line);

    Ok(message.to_string())
}

pub fn parse_file(path: String) -> Result<Vec<Box<ast::Expr>>, Box<dyn std::error::Error>> {
    let path = std::path::Path::new(&path);
    let source = read_to_string(path)?;

    let parser = oters::ProgramParser::new();

    let result = parser.parse(&source);
    
    match result {
        Ok(ast) => Ok (ast),
        Err(e) => match e {
            ParseError::ExtraToken { token } => 
                Err(format!("Extra token {}", get_error_message(&source, token.0)?))?,
            ParseError::InvalidToken { location } => 
                Err(format!("Invalid token {}", get_error_message(&source, location)?))?,
            ParseError::UnrecognizedToken { token, .. } => 
                Err(format!("Unrecognized token {}", get_error_message(&source, token.0)?))?,
            e => 
                Err(format!("Parse error {}", e))?,
        }
    }
}
