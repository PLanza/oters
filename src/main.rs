#![allow(dead_code)]

mod exprs;
mod interpret;
mod parser;
mod types;

use anyhow::Result;
use interpret::{Interpreter, Store};
use types::check::ProgramChecker;

fn main() -> Result<()> {
    let program = parser::parse_file("examples/ex2.otrs".to_string())?;
    let mut checker = ProgramChecker::new();

    let exprs = checker.type_check_program(&program)?;

    println!("{:?}", exprs);
    let mut interpreter = Interpreter::new();
    let (v, (e, s)) = interpreter.step(exprs[0].clone(), Store::new())?;
    println!("{:?}", v);
    println!("{:?}", e);
    println!("{:?}", s);
    let (v, (e, s)) = interpreter.step(e, s)?;
    println!("{:?}", v);
    println!("{:?}", e);
    println!("{:?}", s);

    Ok(())
}
