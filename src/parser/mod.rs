mod ast;
mod tests;

use lalrpop_util::{lalrpop_mod, ParseError};
lalrpop_mod!(pub oters);

use std::fs::read_to_string;
use std::error::Error;

fn parsing_error_message(source: &String, location: (usize, usize))-> Result<String, Box<dyn Error>> {
    let mut line_number = 1;
    let mut line_start = 0;
    let mut line_end = 0;

    // The token's position within its line by number of characters
    let mut token_pos = 0;
    let mut char_count = 0;

    for (i, c) in source.char_indices() {
        if c == '\n' && i < location.0 {
            line_start = i + 1;
            line_number += 1;
            continue;
        }

        if i == location.0 {
            token_pos = char_count;
        }

        if c == '\n' && i >= location.0 {
            line_end = i;
            break;
        }

        char_count += 1;
    }

    // The source code line where the error occurs   
    let code_line = &source[line_start..line_end];

    // A line pointing where the error is located 
    // Assumes all utf8 characters are displayed with the same width
    let mut under_line = (0..token_pos).map(|_| " ").collect::<String>();
    under_line.push_str(&String::from_utf8(vec![b'^'; source[location.0 .. location.1].len()])?);

    let message = format!(
        "on line {}\n{}\n{}", line_number, code_line, under_line);

    Ok(message.to_string())
}

pub fn parse_file(path: String) -> Result<Vec<Box<ast::Expr>>, Box<dyn Error>> {
    let path = std::path::Path::new(&path);
    let source = read_to_string(path)?;

    let parser = oters::ProgramParser::new();

    let result = parser.parse(&source);
    
    match result {
        Ok(ast) => Ok (ast),
        Err(e) => match e {
            ParseError::ExtraToken { token } => 
                Err(format!("Extra token {}", parsing_error_message(&source, (token.0, token.2))?))?,
            ParseError::InvalidToken { location } => 
                Err(format!("Invalid token {}", parsing_error_message(&source, (location, location + 1))?))?,
            ParseError::UnrecognizedToken { token, .. } => 
                Err(format!("Unrecognized token {}", parsing_error_message(&source, (token.0, token.2))?))?,
            e => 
                Err(format!("Parse error {}", e))?,
        }
    }
}
