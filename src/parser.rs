use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub oters);

pub fn parse() {
    let result = oters::OpExprParser::new().parse(r#""hello\"world\"" + 3"#);
    println!("{:?}", result);
}
