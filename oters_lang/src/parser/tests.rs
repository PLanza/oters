#[allow(unused_imports)]
use super::oters;
#[allow(unused_imports)]
use super::span::{SpPExpr, SpPattern, SpTypeExpr, Spanned};

#[allow(unused_imports)]
use std::boxed::Box;

impl<T> Spanned<T> {
    pub(crate) fn unspanned(t: T) -> Self {
        super::span::Spanned {
            span: (0, 0),
            term: Box::new(t),
        }
    }
}

#[cfg(test)]
fn values() -> Vec<(String, SpPExpr)> {
    use super::ast::PExpr::*;
    vec![
        ("true".to_string(), SpPExpr::unspanned(Bool(true))),
        ("false".to_string(), SpPExpr::unspanned(Bool(false))),
        ("1".to_string(), SpPExpr::unspanned(Int(1))),
        ("-10".to_string(), SpPExpr::unspanned(Int(-10))),
        ("1.0".to_string(), SpPExpr::unspanned(Float(1.0))),
        ("-1.0".to_string(), SpPExpr::unspanned(Float(-1.0))),
        (
            r#""Hello""#.to_string(),
            SpPExpr::unspanned(String("Hello".to_string())),
        ),
        (
            r#""He said \"Hi\"""#.to_string(),
            SpPExpr::unspanned(String("He said \\\"Hi\\\"".to_string())),
        ),
        ("()".to_string(), SpPExpr::unspanned(Unit)),
        (
            "var".to_string(),
            SpPExpr::unspanned(Var(Vec::new(), "var".to_string())),
        ),
        (
            "( Option::None )".to_string(),
            SpPExpr::unspanned(Variant(
                vec!["Option".to_string()],
                "None".to_string(),
                None,
            )),
        ),
    ]
}

#[test]
fn test_values() {
    let parser = super::oters::ExprParser::new();

    for v in values() {
        let result = parser.parse(&v.0);
        assert_eq!(result.unwrap(), *v.1.term);
    }
}

#[cfg(test)]
fn unops() -> Vec<(String, SpPExpr)> {
    use super::ast::Opcode::*;
    let unops = vec![
        ("~".to_string(), Neg),
        ("!".to_string(), Not),
        ("@".to_string(), Delay),
        ("#".to_string(), Stable),
        ("!@".to_string(), Adv),
        ("!#".to_string(), Unbox),
        ("out ".to_string(), Out),
        ("into ".to_string(), Into),
    ];

    let mut unop_exprs = Vec::new();

    for op in unops {
        for v in values() {
            use super::ast::PExpr::UnOp;
            let code = format!("{}{}", op.0, v.0);

            let expr = SpPExpr::unspanned(UnOp(op.1, v.1));

            unop_exprs.push((code, expr));
        }
    }

    unop_exprs
}

#[test]
fn test_unops() {
    let parser = super::oters::ExprParser::new();

    for expr in unops() {
        let result = parser.parse(&expr.0);
        assert_eq!(result.unwrap(), *expr.1.term);
    }
}

#[cfg(test)]
fn binops() -> Vec<(String, SpPExpr)> {
    use super::ast::Opcode::*;
    let binops = vec![
        (" + ".to_string(), Add),
        (" - ".to_string(), Sub),
        (" / ".to_string(), Div),
        (" * ".to_string(), Mul),
        (" : ".to_string(), Cons),
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
                use super::ast::PExpr::BinOp;
                let code = format!("{}{}{}", v1.0, op.0, v2.0);

                let expr = SpPExpr::unspanned(BinOp(v1.1.clone(), op.1, v2.1));

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
        assert_eq!(result.unwrap(), *expr.1.term);
    }
}

#[cfg(test)]
fn struct_val() -> (String, SpPExpr) {
    use super::ast::PExpr::StructExpr;

    let mut code = "MyStruct { ".to_string();
    let mut struct_vec = Vec::new();

    for (i, expr) in binops().iter().enumerate() {
        let field = format!("a{}", i);
        let item_code = format!("{}: {},\n", field, expr.0);

        code.push_str(&item_code);

        struct_vec.push((field, expr.1.clone()));
    }

    code.push_str("}");

    (
        code,
        SpPExpr::unspanned(StructExpr(Vec::new(), "MyStruct".to_string(), struct_vec)),
    )
}

#[test]
fn test_struct_val() {
    let parser = super::oters::ExprParser::new();

    let struct_val = struct_val();

    let result = parser.parse(&struct_val.0);
    assert_eq!(result.unwrap(), *struct_val.1.term);
}

#[cfg(test)]
fn tuple() -> (String, SpPExpr) {
    use super::ast::PExpr::Tuple;

    let mut code = "(".to_string();
    let mut tuple_vec = Vec::new();

    for expr in binops() {
        code.push_str(format!("{}, ", expr.0).as_str());

        tuple_vec.push(expr.1.clone());
    }

    code.push_str(")");

    (code, SpPExpr::unspanned(Tuple(tuple_vec)))
}

#[test]
fn test_tuple() {
    let parser = super::oters::ExprParser::new();

    let tuple = tuple();

    let result = parser.parse(&tuple.0);
    assert_eq!(result.unwrap(), *tuple.1.term);
}

#[cfg(test)]
fn list() -> (String, SpPExpr) {
    use std::collections::VecDeque;

    use super::ast::PExpr::List;

    let mut code = "[".to_string();
    let mut list_vec = VecDeque::new();

    for expr in binops() {
        code.push_str(format!("{}, ", expr.0).as_str());

        list_vec.push_back(expr.1.clone());
    }

    code.push_str("]");

    (code, SpPExpr::unspanned(List(list_vec)))
}

#[test]
fn test_list() {
    let parser = super::oters::ExprParser::new();

    let list = list();

    let result = parser.parse(&list.0);
    assert_eq!(result.unwrap(), *list.1.term);
}

#[cfg(test)]
fn primitive_types() -> Vec<(String, SpTypeExpr)> {
    use super::ast::TypeExpr::{TEBool, TEFloat, TEInt, TEString, TEUnit, TEUser};
    vec![
        ("bool".to_string(), SpTypeExpr::unspanned(TEBool)),
        ("int".to_string(), SpTypeExpr::unspanned(TEInt)),
        ("float".to_string(), SpTypeExpr::unspanned(TEFloat)),
        ("string".to_string(), SpTypeExpr::unspanned(TEString)),
        ("()".to_string(), SpTypeExpr::unspanned(TEUnit)),
        (
            "T".to_string(),
            SpTypeExpr::unspanned(TEUser(Vec::new(), "T".to_string(), Vec::with_capacity(0))),
        ),
    ]
}

#[test]
fn test_primitive_types() {
    let parser = super::oters::SpTypeParser::new();

    for t in primitive_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn list_types() -> Vec<(String, SpTypeExpr)> {
    use super::ast::TypeExpr::TEList;
    let mut list_types = Vec::new();
    for t in primitive_types() {
        list_types.push((format!("[{}]", t.0), SpTypeExpr::unspanned(TEList(t.1))));
    }

    list_types
}

#[test]
fn test_list_types() {
    let parser = super::oters::SpTypeParser::new();

    for t in list_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn prefix_types() -> Vec<(String, SpTypeExpr)> {
    use super::ast::TypeExpr::{TEDelay, TEStable};
    let mut types = Vec::new();
    for t in primitive_types() {
        types.push((
            format!("@{}", t.0),
            SpTypeExpr::unspanned(TEDelay(t.1.clone())),
        ));
        types.push((format!("#{}", t.0), SpTypeExpr::unspanned(TEStable(t.1))));
    }

    types
}
#[test]
fn test_prefix_types() {
    let parser = super::oters::SpTypeParser::new();

    for t in prefix_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn function_types() -> Vec<(String, SpTypeExpr)> {
    use super::ast::TypeExpr::TEFunction;
    let mut function_types = Vec::new();

    let mut types = primitive_types();
    types.append(&mut prefix_types());
    types.append(&mut list_types());

    for t1 in types.iter() {
        for t2 in types.iter() {
            let code = format!("{} -> {}", t1.0, t2.0);

            let expr = SpTypeExpr::unspanned(TEFunction(t1.1.clone(), t2.1.clone()));

            function_types.push((code, expr));
        }
    }

    function_types
}

#[test]
fn test_function_types() {
    let parser = super::oters::SpTypeParser::new();

    for t in function_types() {
        let result = parser.parse(&t.0);
        assert_eq!(result.unwrap(), t.1)
    }
}

#[cfg(test)]
fn user_type() -> (String, SpTypeExpr) {
    use super::ast::TypeExpr::TEUser;

    let mut types = primitive_types();
    types.append(&mut prefix_types());
    types.append(&mut list_types());
    types.append(&mut function_types());

    let mut code = "MyType <".to_string();
    let mut generics_vec = Vec::new();

    for t in types {
        code.push_str(format!("{}, ", t.0).as_str());

        generics_vec.push(t.1.clone());
    }

    code.pop();
    code.pop();
    code.push_str(">");

    (
        code,
        SpTypeExpr::unspanned(TEUser(Vec::new(), "MyType".to_string(), generics_vec)),
    )
}

#[test]
fn test_user_type() {
    let parser = super::oters::SpTypeParser::new();

    let user_type = user_type();

    let result = parser.parse(&user_type.0);
    assert_eq!(result.unwrap(), user_type.1);
}

#[cfg(test)]
fn types() -> Vec<(String, SpTypeExpr)> {
    let mut types = primitive_types();
    types.append(&mut prefix_types());
    types.append(&mut list_types());
    types.append(&mut function_types());
    types.push(user_type());

    types
}

#[test]
fn test_type_alias() {
    use super::ast::PExpr::TypeDef;

    let params = vec!["A".to_string(), "B".to_string()];

    let parser = super::oters::ItemParser::new();

    for t in types() {
        let code = format!("type MyAlias<A, B> = {}", t.0);

        let result = parser.parse(&code);
        assert_eq!(
            result.unwrap(),
            TypeDef("MyAlias".to_string(), params.clone(), t.1.clone())
        );
    }
}

#[test]
fn test_struct_def() {
    use super::ast::PExpr::StructDef;

    let params = vec!["A".to_string(), "B".to_string()];

    let parser = super::oters::ItemParser::new();

    let mut code = "struct MyStruct<A, B> {".to_string();
    let mut fields = Vec::new();

    for (i, t) in types().iter().enumerate() {
        code.push_str(format!("a{} : {},\n", i, t.0).as_str());
        fields.push((format!("a{}", i).to_string(), t.1.clone()));
    }

    code.push_str("}");

    let result = parser.parse(&code);
    assert_eq!(
        result.unwrap(),
        StructDef("MyStruct".to_string(), params.clone(), fields)
    );
}

#[test]
fn test_enum_def() {
    use super::ast::PExpr::EnumDef;

    let params = vec!["A".to_string(), "B".to_string()];

    let parser = super::oters::ItemParser::new();

    let mut code = "enum MyEnum<A, B> {".to_string();
    let mut fields = Vec::new();

    for (i, t) in types().iter().enumerate() {
        code.push_str(format!("A{} {},\n", i, t.0).as_str());
        fields.push((format!("A{}", i).to_string(), Some(t.1.clone())));
    }

    fields.append(&mut vec![
        ("Empty1".to_string(), None),
        ("Empty2'".to_string(), None),
        ("Empty_3".to_string(), None),
    ]);
    code.push_str("Empty1, Empty2', Empty_3 }");

    let result = parser.parse(&code);
    assert_eq!(
        result.unwrap(),
        EnumDef("MyEnum".to_string(), params.clone(), fields)
    );
}

#[cfg(test)]
fn ifs() -> Vec<(String, SpPExpr)> {
    use super::ast::PExpr::If;

    let mut exprs = binops();
    exprs.append(&mut unops());
    exprs.append(&mut values());

    let mut if_vecs = Vec::new();
    let mut i = 0;

    while i < exprs.len() - 3 {
        let code = format!(
            "if {} then \n\t {} \n else \n\t {}",
            exprs[i].0,
            exprs[i + 1].0,
            exprs[i + 2].0
        );
        if_vecs.push((
            code.to_string(),
            SpPExpr::unspanned(If(
                exprs[i].1.clone(),
                exprs[i + 1].1.clone(),
                exprs[i + 2].1.clone(),
            )),
        ));

        i += 3;
    }

    if_vecs
}

#[test]
fn test_if_exprs() {
    let parser = super::oters::ExprParser::new();

    for v in ifs() {
        let result = parser.parse(&v.0);
        assert_eq!(result.unwrap(), *v.1.term);
    }
}

#[cfg(test)]
fn block_expr() -> (String, SpPExpr) {
    use super::ast::PExpr::Block;

    let mut exprs = binops();
    exprs.append(&mut unops());
    exprs.append(&mut values());
    exprs.append(&mut ifs());

    let mut expr_vec = Vec::new();
    let mut code = "{".to_string();

    for e in exprs {
        code.push_str(&e.0);
        code.push_str(";\n");

        expr_vec.push(e.1.clone());
    }

    code.pop();
    code.pop();
    code.push('}');

    (code, SpPExpr::unspanned(Block(expr_vec)))
}

#[test]
fn test_block_expr() {
    let parser = super::oters::ExprParser::new();

    let block_expr = block_expr();

    let result = parser.parse(&block_expr.0);
    assert_eq!(result.unwrap(), *block_expr.1.term);
}

#[cfg(test)]
fn functions() -> Vec<(String, SpPExpr)> {
    use super::ast::PExpr::Fn;
    use super::ast::Pattern::{Stream, Var};

    let mut exprs = binops();
    exprs.append(&mut unops());
    exprs.append(&mut ifs());
    exprs.push(block_expr());

    let mut fn_vecs = Vec::new();

    for e in exprs {
        let code = format!("fn (x << xs) #y -> {}", e.0);

        fn_vecs.push((
            code.to_string(),
            SpPExpr::unspanned(Fn(
                vec![
                    SpPattern::unspanned(Stream(
                        SpPattern::unspanned(Var("x".to_string(), false)),
                        SpPattern::unspanned(Var("xs".to_string(), false)),
                    )),
                    SpPattern::unspanned(Var("y".to_string(), true)),
                ],
                e.1.clone(),
            )),
        ));
    }

    fn_vecs
}

#[test]
fn test_fn_exprs() {
    let parser = super::oters::ExprParser::new();

    for v in functions() {
        let result = parser.parse(&v.0);
        assert_eq!(result.unwrap(), *v.1.term);
    }
}

#[cfg(test)]
fn variant_exprs() -> Vec<(String, SpPExpr)> {
    use super::ast::PExpr::Variant;

    let mut exprs = binops();
    exprs.append(&mut unops());

    let mut variants_vec = Vec::new();

    for e in exprs {
        let code = format!("MyEnum::Constr({})", e.0);

        variants_vec.push((
            code.to_string(),
            SpPExpr::unspanned(Variant(
                vec!["MyEnum".to_string()],
                "Constr".to_string(),
                Some(e.1.clone()),
            )),
        ));
    }

    variants_vec.push((
        "MyEnum::Constr".to_string(),
        SpPExpr::unspanned(Variant(
            vec!["MyEnum".to_string()],
            "Constr".to_string(),
            None,
        )),
    ));

    variants_vec
}

#[test]
fn test_variant_exprs() {
    let parser = super::oters::ExprParser::new();

    for v in variant_exprs() {
        let result = parser.parse(&v.0);
        assert_eq!(result.unwrap(), *v.1.term);
    }
}
