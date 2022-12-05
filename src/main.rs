#![allow(dead_code)]

mod exprs;
mod parser;
mod types;

use anyhow::Result;
use types::check::ProgramChecker;

fn main() -> Result<()> {
    let program = parser::parse_file("examples/ex1.otrs".to_string())?;
    let mut checker = ProgramChecker::new();

    checker.type_check_program(&program)?;
    println!("{:?}", checker);

    Ok(())
}
