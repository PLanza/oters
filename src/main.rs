mod parser;

use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    parser::parse_file("examples/ex1.otrs".to_string())?;
    let result = parser::parse_file("examples/nonex1.otrs".to_string());

    match result {
        Err(e) => println!("{}", e),
        _ => (),
    }

    Ok(())
}
