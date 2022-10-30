#[cfg(test)]

#[allow(unused_imports)]
use super::oters;

#[allow(unused_imports)]
use std::boxed::Box;

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
        (r#""He said \"Hi\"""#.to_string(), Box::new(String("He said \\\"Hi\\\"".to_string()))),
        ("()".to_string(), Box::new(Unit)), 
        ("var".to_string(), Box::new(Var("var".to_string()))),
        ("(None)".to_string(), Box::new(Variant("None".to_string(), None))),
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

fn unops() -> Vec<(String, Box<super::ast::Expr>)> {
    use super::ast::Opcode::*;
    let unops = vec![
        ("~".to_string(), Neg), 
        ("!".to_string(), Not), 
        ("@".to_string(), Delay), 
        ("#".to_string(), Box), 
        ("!@".to_string(), Adv),
        ("!#".to_string(), Unbox),
        ("out ".to_string(), Out), 
        ("into ".to_string(), Into), 
    ];

    let mut unop_exprs = Vec::new();

    for op in unops {
        for v in values() {
            use super::ast::Expr::UnOp;
            let code = format!("{}{}", op.0, v.0);

            let expr = std::boxed::Box::new(UnOp(op.1, v.1));

            unop_exprs.push((code, expr));
        }
    }

    unop_exprs
}

#[test]
fn test_unops() {
    let parser = super::oters::ExprParser::new();
    
    for expr in binops() {
        let result = parser.parse(&expr.0);
        assert_eq!(result.unwrap(), expr.1);
    }
}

fn binops() -> Vec<(String, Box<super::ast::Expr>)> {
    use super::ast::Opcode::*;
    let binops = vec![
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
    ];

    let mut binop_exprs = Vec::new();

    for op in binops {
        for v1 in unops() {
            for v2 in unops() {
                use super::ast::Expr::BinOp;
                let code = format!("{}{}{}", v1.0, op.0, v2.0);

                let expr = std::boxed::Box::new(BinOp(v1.1.clone(), op.1, v2.1));

                binop_exprs.push((code, expr));
            }
        }
    }

    binop_exprs
}

#[test]
fn test_binops() {
    let parser = super::oters::ExprParser::new();
    
    for expr in binops() {
        let result = parser.parse(&expr.0);
        assert_eq!(result.unwrap(), expr.1);
    }
}

fn struct_val() -> (String, Box<super::ast::Expr>) {
    use super::ast::Expr::StructVal;
    
    let mut code = "MyStruct { ".to_string();
    let mut struct_vec = Vec::new();

    for (i, expr) in binops().iter().enumerate() {
        let field = format!("a{}", i); 
        let item_code = format!("{}: {},\n", field, expr.0);

        code.push_str(&item_code);

        struct_vec.push((field, expr.1.clone()));
    }

    code.push_str("}");


    (code, Box::new(StructVal("MyStruct".to_string(), struct_vec)))
}

#[test]
fn test_struct_val() {
    let parser = super::oters::ExprParser::new();

    let struct_val = struct_val();
    
    let result = parser.parse(&struct_val.0);
    assert_eq!(result.unwrap(), struct_val.1);
}

fn tuple() -> (String, Box<super::ast::Expr>) {
    use super::ast::Expr::Tuple;
    
    let mut code = "(".to_string();
    let mut tuple_vec = Vec::new();

    for expr in binops() {
        code.push_str(format!("{}, ", expr.0).as_str());

        tuple_vec.push(expr.1.clone());
    }

    code.push_str(")");

    (code, Box::new(Tuple(tuple_vec)))
}

#[test]
fn test_tuple() {
    let parser = super::oters::ExprParser::new();

    let tuple = tuple();
    
    let result = parser.parse(&tuple.0);
    assert_eq!(result.unwrap(), tuple.1);
}


fn list() -> (String, Box<super::ast::Expr>) {
    use super::ast::Expr::List;
    
    let mut code = "[".to_string();
    let mut list_vec = Vec::new();

    for expr in binops() {
        code.push_str(format!("{}, ", expr.0).as_str());

        list_vec.push(expr.1.clone());
    }

    code.push_str("]");

    (code, Box::new(List(list_vec)))
}

#[test]
fn test_list() {
    let parser = super::oters::ExprParser::new();

    let list = list();
    
    let result = parser.parse(&list.0);
    assert_eq!(result.unwrap(), list.1);
}


