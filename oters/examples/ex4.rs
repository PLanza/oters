extern crate oters;

use anyhow::Result;
use oters::interpret::Interpreter;
use oters::parser;
use oters::types::check::ProgramChecker;

fn main() -> Result<()> {
    let program = parser::parse_file("oters/examples/ex4.otrs".to_string())?;
    let mut checker = ProgramChecker::new();

    let exprs = checker.type_check_program(&program)?;

    let mut interpreter = Interpreter::new();
    interpreter.init(exprs)?;
    interpreter.eval_loop()?;

    Ok(())
}
