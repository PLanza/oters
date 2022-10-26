mod parser;

use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    parser::parse();

    Ok(())
}
