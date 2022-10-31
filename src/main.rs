mod parser;

use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    parser::parse_file("examples/ex1.otrs".to_string())?;

    Ok(())
}
