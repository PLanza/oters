mod ast;
mod tests;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub oters);

pub fn parse() {
    let parser = oters::ExprParser::new();

    let result = parser.parse("! None");

    println!("{:?}", result);
}
