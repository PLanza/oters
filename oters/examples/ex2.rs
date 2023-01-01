extern crate oters;

use std::collections::HashMap;

use anyhow::Result;
use oters::interpret::Interpreter;
use oters::parser;
use oters::types::check::ProgramChecker;

fn main() -> Result<()> {
    let program = parser::parse_file("oters/examples/ex2.otrs".to_string())?;
    let mut checker = ProgramChecker::new(HashMap::new());

    let exprs = checker.type_check_program(&program)?;

    let mut interpreter = Interpreter::new(exprs, HashMap::new())?;
    interpreter.eval_loop()?;

    Ok(())
}
