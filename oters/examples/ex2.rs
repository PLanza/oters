extern crate oters;

use anyhow::Result;
use oters::export::{export_list, export_oters};
use oters::interpret::Interpreter;
use oters::parser;
use oters::types::check::ProgramChecker;

#[export_oters]
fn print_int(i: i64) {
    println!("{}", i);
}

export_list!();

fn main() -> Result<()> {
    let program = parser::parse_file("oters/examples/ex2.otrs".to_string())?;
    let mut checker = ProgramChecker::new();

    checker.type_check_program(
        &program,
        vec!["ex2".to_string()],
        Some((
            EXPORT_FNS.clone(),
            EXPORT_STRUCTS.clone(),
            EXPORT_ENUMS.clone(),
        )),
    )?;

    let mut interpreter = Interpreter::new(
        checker.checked_exprs,
        EXPORT_FNS.clone(),
        vec!["ex2".to_string()],
    )?;
    loop {
        interpreter.eval_step()?;
    }
}
