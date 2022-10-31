mod ast;
mod tests;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub oters);

use std::fs::read_to_string;

pub fn parse_file(path: String) -> Result<(), Box<dyn std::error::Error>> {
    let path = std::path::Path::new(&path);
    let source = read_to_string(path)?;

    let parser = oters::ProgramParser::new();

    let result = parser.parse(&source);

    println!("{:?}", result);

    Ok(())
}
