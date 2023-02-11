extern crate oters;

use std::fs::read_to_string;

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
    let source = read_to_string(std::path::Path::new("oters/examples/ex1.otrs"))?;
    let program = parser::parse_source(source.clone()).map_err(|e| e.to_anyhow(&source))?;
    let mut checker = ProgramChecker::new();

    checker
        .type_check_program(
            &program,
            vec!["ex1".to_string()],
            Some((
                EXPORT_FNS.clone(),
                EXPORT_STRUCTS.clone(),
                EXPORT_ENUMS.clone(),
            )),
        )
        .map_err(|e| e.to_anyhow(&source))?;

    let mut interpreter = Interpreter::new(
        checker.checked_exprs,
        EXPORT_FNS
            .clone()
            .into_iter()
            .map(|(name, val)| ((vec!["ex1".to_string()], name), val))
            .collect(),
        vec!["ex1".to_string()],
    )
    .map_err(|e| e.to_anyhow(&source))?;
    loop {
        interpreter.eval_step().map_err(|e| e.to_anyhow(&source))?;
    }
}
