#![allow(dead_code)]

mod parser;
// mod exprs;
// mod types;

use anyhow::Result;

fn main() -> Result<()> {
    let program = parser::parse_file("examples/ex1.otrs".to_string())?;

    let result = parser::parse_file("examples/nonex1.otrs".to_string());

    match result {
        Err(e) => println!("{}", e),
        _ => (),
    }

    parser::parse_file("examples/t1.otrs".to_string())?;

    Ok(())
}
