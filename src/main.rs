#![allow(dead_code)]

mod parser;
mod types;

use anyhow::Result;

use crate::types::check::ProgramChecker;

fn main() -> Result<()> {
    let program = parser::parse_file("examples/ex1.otrs".to_string())?;
    println!("{:?}", program);

    let result = parser::parse_file("examples/nonex1.otrs".to_string());

    match result {
        Err(e) => println!("{}", e),
        _ => (),
    }

    let program2 = parser::parse_file("examples/t1.otrs".to_string())?;

    let mut type_checker = ProgramChecker::new();
    type_checker.type_check_program(&program2)?;

    Ok(())
}
