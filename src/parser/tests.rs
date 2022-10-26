#[cfg(test)]
use super::ast::{Expr::*, Opcode::*};

#[allow(unused_imports)]
use super::oters;

#[allow(unused_imports)]
use std::boxed::Box;

#[test]
fn values() {
    let result = oters::ExprParser::new().parse("true");
    assert_eq!(result.unwrap(), Box::new(True));
    let result = oters::ExprParser::new().parse("false");
    assert_eq!(result.unwrap(), Box::new(False));
    let result = oters::ExprParser::new().parse("-4000");
    assert_eq!(result.unwrap(), Box::new(Int(-4000)));
    let result = oters::ExprParser::new().parse("-3.1415");
    assert_eq!(result.unwrap(), Box::new(Float(-3.1415)));
    let result = oters::ExprParser::new().parse(r#""And he said, \" Hello! \" which I ignored.""#);
    assert_eq!(
        result.unwrap(),
        Box::new(String(
            r#"And he said, \" Hello! \" which I ignored."#.to_string()
        ))
    );
    let result = oters::ExprParser::new().parse("()");
    assert_eq!(result.unwrap(), Box::new(Unit));
    let result = oters::ExprParser::new().parse("[]");
    assert_eq!(result.unwrap(), Box::new(Nil));
}

#[test]
fn binary_ops() {
    let result = oters::ExprParser::new().parse("(-4.2 * 3. - 7. :: [])");
    assert_eq!(
        result.unwrap(),
        Box::new(BinOp(
            Box::new(BinOp(
                Box::new(BinOp(Box::new(Float(-4.2)), Mul, Box::new(Float(3.)))),
                Sub,
                Box::new(Float(7.))
            )),
            Cons,
            Box::new(Nil)
        ))
    );
}

#[test]
fn structs() {
    let result = oters::ExprParser::new().parse(
        "MyStruct {
            some_int: 3+7,
            some_string: \"Hello\"
        }",
    );
    assert_eq!(
        result.unwrap(),
        Box::new(StructVal(
            "MyStruct".to_string(),
            vec![
                (
                    "some_int".to_string(),
                    Box::new(BinOp(Box::new(Int(3)), Add, Box::new(Int(7))))
                ),
                (
                    "some_string".to_string(),
                    Box::new(String("Hello".to_string()))
                )
            ]
        ))
    );
}

#[test]
fn let_expr() {
    let result = oters::ItemParser::new().parse("let val = MyStruct { list: 20::[], };");
    assert_eq!(
        result.unwrap(),
        Box::new(Let(
            "val".to_string(),
            Box::new(StructVal(
                "MyStruct".to_string(),
                vec![(
                    "list".to_string(),
                    Box::new(BinOp(Box::new(Int(20)), Cons, Box::new(Nil)))
                )]
            ))
        ))
    );
}
