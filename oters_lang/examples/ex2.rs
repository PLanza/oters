extern crate oters_lang;

use oters_lang as oters;

use anyhow::Result;
use oters_lang::export::{export_list, export_oters};
use oters_lang::interpret::Interpreter;
use oters_lang::parser;
use oters_lang::types::check::ProgramChecker;

#[export_oters]
fn print_int(i: i64) {
    println!("{}", i);
}

#[export_oters]
fn contains(xs: Vec<i64>, v: i64) -> bool {
    xs.into_iter().find(|x| x == &v).is_some()
}

export_list!();

fn main() -> Result<()> {
    let source = std::fs::read_to_string(std::path::Path::new("examples/ex2.otrs"))?;
    let program = parser::parse_source(source.clone()).map_err(|e| e.to_anyhow(&source))?;
    let mut checker = ProgramChecker::new();
    println!("{:?}", EXPORT_FNS.clone());

    checker
        .type_check_program(
            &program,
            vec!["ex2".to_string()],
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
            .map(|(name, val)| ((vec!["ex2".to_string()], name), val))
            .collect(),
        vec!["ex2".to_string()],
    )
    .map_err(|e| e.to_anyhow(&source))?;
    loop {
        interpreter.eval_step().map_err(|e| e.to_anyhow(&source))?;
    }
}
