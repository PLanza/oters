#![allow(dead_code)]

pub mod exprs;
pub mod interpret;
pub mod parser;
pub mod types;

use anyhow::Result;
use interpret::Interpreter;
use types::check::ProgramChecker;

fn main() -> Result<()> {
    let program = parser::parse_file("examples/ex2.otrs".to_string())?;
    let mut checker = ProgramChecker::new();

    let exprs = checker.type_check_program(&program)?;

    let mut interpreter = Interpreter::new();
    interpreter.init(exprs)?;
    interpreter.eval_loop()?;

    Ok(())
}
