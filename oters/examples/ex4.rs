extern crate oters;

use anyhow::Result;
use oters::export::{export_fn, export_list};
use oters::interpret::Interpreter;
use oters::parser;
use oters::types::check::ProgramChecker;

#[export_fn]
fn print_int(i: i64) {
    println!("{}", i);
}

export_list!();

fn main() -> Result<()> {
    let program = parser::parse_file("oters/examples/ex4.otrs".to_string())?;
    let mut checker = ProgramChecker::new(EXPORT_FNS.clone());

    let exprs = checker.type_check_program(&program)?;

    let mut interpreter = Interpreter::new(exprs, EXPORT_FNS.clone())?;
    interpreter.eval_loop()?;

    Ok(())
}
