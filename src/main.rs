mod parser;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    parser::parse("examples/ex0.otrs")?;

    Ok(())
}
