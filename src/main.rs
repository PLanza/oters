#![allow(dead_code)]

mod exprs;
mod parser;
mod types;

use anyhow::Result;
use types::check::ProgramChecker;

fn main() -> Result<()> {
    let program = parser::parse_file("examples/infer.otrs".to_string())?;
    let mut checker = ProgramChecker::new();

    match *program[0].clone() {
        crate::parser::ast::PExpr::Let(_, e) => {
            let e = exprs::Expr::from_pexpr(*e)?;
            println!("{:?}", e);
            let t = checker.infer(&e, exprs::VarContext::new())?;
            println!("{:?}", t);
        }
        _ => (),
    }
    Ok(())
}
