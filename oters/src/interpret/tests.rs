#![allow(unused_imports)]

use super::Interpreter;
use crate as oters;
use crate::parser;
use crate::ProgramChecker;

#[crate::export::export_oters]
fn print_int(i: i64) {
    println!("{}", i);
}

crate::export::export_list!();

#[test]
fn evaluate_examples() {
    let program =
        parser::parse_source(include_str!("../../examples/ex2.otrs").to_string()).unwrap();
    let mut checker = ProgramChecker::new();

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
        .unwrap();

    let mut interpreter = Interpreter::new(
        checker.checked_exprs,
        EXPORT_FNS
            .clone()
            .into_iter()
            .map(|(name, val)| ((vec!["ex2".to_string()], name), val))
            .collect(),
        vec!["ex2".to_string()],
    )
    .unwrap();
    for _ in 0..10 {
        interpreter.eval_step().unwrap();
    }

    let program =
        parser::parse_source(include_str!("../../examples/ex1.otrs").to_string()).unwrap();
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
        .unwrap();

    let mut interpreter = Interpreter::new(
        checker.checked_exprs,
        EXPORT_FNS
            .clone()
            .into_iter()
            .map(|(name, val)| ((vec!["ex1".to_string()], name), val))
            .collect(),
        vec!["ex1".to_string()],
    )
    .unwrap();
    for _ in 0..10 {
        interpreter.eval_step().unwrap();
    }
}
