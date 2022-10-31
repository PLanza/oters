#[allow(unused_imports)]
use super::oters;

#[allow(unused_imports)]
use std::boxed::Box;

#[cfg(test)]
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
        ("( None )".to_string(), Box::new(Variant("None".to_string(), None))),
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

#[cfg(test)]
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

#[cfg(test)]
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

#[cfg(test)]
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

#[cfg(test)]
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

#[cfg(test)]
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

#[cfg(test)]
fn primitive_types() -> Vec<(String, Box<super::ast::Type>)> {
    use super::ast::Type::{Bool, Int, Float, String, Unit, Var, User};
    vec![
        ("bool".to_string(), Box::new(Bool)), 
        ("int".to_string(), Box::new(Int)), 
        ("float".to_string(), Box::new(Float)), 
        ("string".to_string(), Box::new(String)), 
        ("()".to_string(), Box::new(Unit)),
        ("alpha".to_string(), Box::new(Var("alpha".to_string()))),
        ("T".to_string(), Box::new(User("T".to_string(), Vec::with_capacity(0)))),
    ]
}

#[test]
fn test_primitive_types() {
    let parser = super::oters::TypeParser::new(); 

    for t in primitive_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn list_types() -> Vec<(String, Box<super::ast::Type>)> {
    use super::ast::Type::List;
    let mut list_types = Vec::new();
    for t in primitive_types() {
        list_types.push((format!("[{}]", t.0), Box::new(List(t.1))));

    }

    list_types
}

#[test]
fn test_list_types() {
    let parser = super::oters::TypeParser::new(); 

    for t in list_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn prefix_types() -> Vec<(String, Box<super::ast::Type>)> {
    use super::ast::Type::{Box, Delay};
    let mut types = Vec::new();
    for t in primitive_types() {
        types.push((format!("@{}", t.0), std::boxed::Box::new(Delay(t.1.clone()))));
        types.push((format!("#{}", t.0), std::boxed::Box::new(Box(t.1))));
    }

    types

}
#[test]
fn test_prefix_types() {
    let parser = super::oters::TypeParser::new(); 

    for t in prefix_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn function_types() -> Vec<(String, Box<super::ast::Type>)> {
    use super::ast::Type::Function;
    let mut function_types = Vec::new();

    let mut types = primitive_types();
    types.append(&mut prefix_types());
    types.append(&mut list_types());

    for t1 in types.iter() {
        for t2 in types.iter() {
            let code = format!("{} -> {}", t1.0, t2.0);

            let expr = std::boxed::Box::new(Function(t1.1.clone(), t2.1.clone()));

            function_types.push((code, expr));
        }
    }

    function_types
}

#[test]
fn test_function_types() {
    let parser = super::oters::TypeParser::new(); 

    for t in function_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn fix_types() -> Vec<(String, Box<super::ast::Type>)> {
    use super::ast::Type::Fix;

    let mut types = primitive_types();
    types.append(&mut prefix_types());
    types.append(&mut list_types());
    types.append(&mut function_types());

    let mut fix_types = Vec::new();

    for t in types.iter() {
        let code = format!("fix alpha -> {}", t.0);

        let expr = std::boxed::Box::new(Fix("alpha".to_string(), t.1.clone()));

        fix_types.push((code, expr));
    }

    fix_types
}

#[test]
fn test_fix_types() {
    let parser = super::oters::TypeParser::new(); 

    for t in fix_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn user_type() -> (String, Box<super::ast::Type>) {
    use super::ast::Type::User;

    let mut types = primitive_types();
    types.append(&mut prefix_types());
    types.append(&mut list_types());
    types.append(&mut function_types());
    types.append(&mut fix_types());
    
    let mut code = "MyType <".to_string();
    let mut generics_vec = Vec::new();

    for t in types {
        code.push_str(format!("{}, ", t.0).as_str());

        generics_vec.push(t.1.clone());
    }

    code.pop();
    code.pop();
    code.push_str(">");

    (code, Box::new(User("MyType".to_string(), generics_vec)))
}

#[test]
fn test_user_type() {
    let parser = super::oters::TypeParser::new();

    let user_type = user_type();
    
    let result = parser.parse(&user_type.0);
    assert_eq!(result.unwrap(), user_type.1);
}

#[cfg(test)]
fn types() -> Vec<(String, Box<super::ast::Type>)> {
    let mut types = primitive_types();
    types.append(&mut prefix_types());
    types.append(&mut list_types());
    types.append(&mut function_types());
    types.append(&mut fix_types());
    types.push(user_type());

    types
}

#[test]
fn test_type_alias() {
    use super::ast::Expr::Type;

    let params = vec![(true, "A".to_string()), (false, "B".to_string())];

    
    let parser = super::oters::ItemParser::new();
    
    for t in types() {
        let code = format!("type MyAlias<#A, B> = {}", t.0);

        let result = parser.parse(&code);
        assert_eq!(result.unwrap(), Box::new(Type("MyAlias".to_string(), params.clone(), t.1.clone())));
    };
}

#[test]
fn test_struct_def() {
    use super::ast::Expr::Struct;

    let params = vec![(true, "A".to_string()), (false, "B".to_string())];

    let parser = super::oters::ItemParser::new();

    let mut code = "struct MyStruct<#A, B> {".to_string();
    let mut fields = Vec::new();
    
    for (i, t) in types().iter().enumerate() {
        code.push_str(format!("a{} : {},\n", i, t.0).as_str());
        fields.push((format!("a{}", i).to_string(), t.1.clone()));
    };

    code.push_str("}");    

    let result = parser.parse(&code);
    assert_eq!(result.unwrap(), Box::new(Struct("MyStruct".to_string(), params.clone(), fields)));
}

#[test]
fn test_enum_def() {
    use super::ast::Expr::Enum;

    let params = vec![(true, "A".to_string()), (false, "B".to_string())];

    let parser = super::oters::ItemParser::new();

    let mut code = "enum MyEnum<#A, B> {".to_string();
    let mut fields = Vec::new();
    
    for (i, t) in types().iter().enumerate() {
        code.push_str(format!("A{} {},\n", i, t.0).as_str());
        fields.push((format!("A{}", i).to_string(), Some(t.1.clone())));
    };

    fields.append(&mut vec![("Empty1".to_string(), None), ("Empty2'".to_string(), None), ("Empty_3".to_string(), None),]);
    code.push_str("Empty1, Empty2', Empty_3 }");    

    let result = parser.parse(&code);
    assert_eq!(result.unwrap(), Box::new(Enum("MyEnum".to_string(), params.clone(), fields)));
}

#[cfg(test)]
fn functions() -> Vec<(String, Box<super::ast::Expr>)> {
    use super::ast::Expr::Fn;
    
    let mut exprs = binops();
    exprs.append(&mut unops());

    let mut fn_vecs = Vec::new();

    let types = types();

    for (i, t) in types.iter().enumerate() {
        let e = &exprs[i];
        let code = format!("fn (arg : {}) -> {}", t.0, e.0);

        fn_vecs.push((code.to_string(), 
                Box::new(Fn(vec![("arg".to_string(), t.1.clone())], e.1.clone()))));
    } 

    fn_vecs
}

#[test]
fn test_fn_exprs() { 
    let parser = super::oters::ExprParser::new();

    for v in functions() {
        let result = parser.parse(&v.0);
        assert_eq!(result.unwrap(), v.1);
    }
}
