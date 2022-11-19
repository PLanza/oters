#![allow(dead_code)]

mod exprs;
mod parser;
mod types;

use anyhow::Result;

fn main() -> Result<()> {
    let program = parser::parse_file("examples/infer.otrs".to_string())?;
    
    Ok(())
}
