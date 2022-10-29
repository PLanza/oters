#[cfg(test)]

#[allow(unused_imports)]
use super::oters;

#[allow(unused_imports)]
use std::boxed::Box;
use std::result;

fn values() -> Vec<(String, Box<super::ast::Expr>)> {
    use super::ast::Expr::*;
    vec![
        ("true".to_string(), Box::new(True)), 
        ("false".to_string(), Box::new(False)), 
        ("1".to_string(), Box::new(Int(1))), 
        ("-10".to_string(), Box::new(Int(-10))), 
        ("1.0".to_string(), Box::new(Float(1.0))),
        ("-1.0".to_string(), Box::new(Float(-1.0))),
        (r#""Hello""#.to_string(), Box::new(String("Hello".to_string()))),
        (r#""He said \"Hi\"""#.to_string(), Box::new(String("He said \"Hi\"".to_string()))),
        ("()".to_string(), Box::new(Unit)), 
    ]
}

#[test]
fn test_values() {
    let parser = super::oters::ExprParser::new();

    for v in values() {
        let result = parser.parse(&v.0);
        assert_eq!(result.unwrap(), v.1);
    }

}

fn binops() -> Vec<(String, super::ast::Opcode)> {
    use super::ast::Opcode::*;
    vec![
        (" + ".to_string(), Add), 
        (" - ".to_string(), Sub), 
        (" / ".to_string(), Div), 
        (" * ".to_string(), Mul), 
        (" :: ".to_string(), Cons),
        (" << ".to_string(), Stream),
        (" == ".to_string(), Eq), 
        (" < ".to_string(), Lt), 
        (" > ".to_string(), Gt), 
        (" && ".to_string(), And), 
        (" || ".to_string(), Or), 
    ]
}


#[test]
fn test_binops() {
    use super::ast::Expr::BinOp;
    let parser = super::oters::ExprParser::new();
    
    for op in binops() {
        for v1 in values() {
            for v2 in values() {
                let code = format!("{}{}{}", v1.0, op.0, v2.0);

                let result = parser.parse(&code);
                assert_eq!(result.unwrap(), Box::new(BinOp(v1.1.clone(), op.1, v2.1)));
            }
        }
    }

}

/*
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
            Box::new(List(Vec::new()))
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
                    Box::new(BinOp(Box::new(Int(20)), Cons, Box::new(List(Vec::new()))))
                )]
            ))
        ))
    );
}*/
