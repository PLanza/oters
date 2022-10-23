use std::error::Error;

use pest::Parser;

#[derive(Parser)]
#[grammar = "src/grammar.pest"]
struct OtersParser;

pub fn parse(path: &str) -> Result<(), Box<dyn Error>> {
    let source = std::fs::read_to_string(path)?;

    let file = OtersParser::parse(Rule::file, &source);
    match file {
        Err(e) => println!("{:?}", e),
        Ok(mut file) => {
            for record in file.next().unwrap().into_inner() {
                match record.as_rule() {
                    Rule::item => {
                        println!("{:?}", record)
                    }
                    Rule::EOI => (),
                    _ => unreachable!(),
                }
            }
        }
    }

    Ok(())
}
