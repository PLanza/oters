use crate::{parser::ast::Pattern, types::Type};

use super::Expr;

// Single tick translation
#[test]
fn test_single_tick_translation() {
    let program = r"
let test = fn x -> @(!@(@(x + 2)))
";
    let test_code = crate::parser::parse_source(program.to_string()).unwrap();
    let mut checker = crate::types::check::ProgramChecker::new();
    checker
        .type_check_program(&test_code, Vec::new(), None)
        .unwrap();

    let node: daggy::NodeIndex = 0.into();
    let bindings = checker.checked_exprs[node].clone();
    use super::Expr::*;
    use super::LetBinding;
    use super::Pattern;
    assert_eq!(
        bindings[0],
        LetBinding::Let(
            Pattern::Var("test".to_string(), false),
            Fn(
                Pattern::Var("x".to_string(), false),
                Box::new(LetIn(
                    Pattern::Var("_d0".to_string(), false),
                    Box::new(Delay(Box::new(BinOp(
                        Box::new(Var(Vec::new(), "x".to_string())),
                        super::BOpcode::Add,
                        Box::new(Int(2))
                    )))),
                    Box::new(Delay(Box::new(Adv(Box::new(Var(
                        Vec::new(),
                        "_d0".to_string()
                    ))))))
                ))
            )
        )
    );
}

// Pattern variable capturing
#[test]
fn test_pattern_var_capturing() {
    // [Option::Some x, MyStruct { field: y, z, ..}] | (_, a:b) << c
    use crate::parser::ast::Pattern::*;
    let pat = Or(
        Box::new(List(vec![
            Box::new(Variant(
                vec!["Option".to_string()],
                "Some".to_string(),
                Some(Box::new(Var("x".to_string(), false))),
            )),
            Box::new(Struct(
                Vec::new(),
                "MyStruct".to_string(),
                vec![
                    (
                        "field".to_string(),
                        Some(Box::new(Var("y".to_string(), false))),
                    ),
                    ("z".to_string(), None),
                    ("..".to_string(), None),
                ],
            )),
        ])),
        Box::new(Stream(
            Box::new(Tuple(vec![
                Box::new(Underscore),
                Box::new(Cons(
                    Box::new(Var("a".to_string(), false)),
                    Box::new(Var("b".to_string(), false)),
                )),
            ])),
            Box::new(Var("c".to_string(), false)),
        )),
    );

    let vars = vec!["x", "y", "z", "a", "b", "c"];
    for var in vars.clone() {
        assert!(pat.contains(&var.to_string()));
    }

    assert_eq!(pat.vars(), vars);
}

#[test]
fn test_guarded_recursion() {
    // const = fn x -> x << @(const x)
    let program = r"
let const = fn #x -> x << @(const x)
";
    let test_code = crate::parser::parse_source(program.to_string()).unwrap();
    let mut checker = crate::types::check::ProgramChecker::new();
    checker
        .type_check_program(&test_code, Vec::new(), None)
        .unwrap();

    let node: daggy::NodeIndex = 0.into();
    let bindings = checker.checked_exprs[node].clone();
    use super::Expr::*;
    use super::LetBinding;
    use super::Pattern;
    assert_eq!(
        bindings[0],
        LetBinding::Let(
            Pattern::Var("const".to_string(), false),
            Fix(
                "rec_const".to_string(),
                Box::new(Fn(
                    Pattern::Var("x".to_string(), true),
                    Box::new(Into(Box::new(Tuple(vec![
                        Box::new(Var(Vec::new(), "x".to_string())),
                        Box::new(LetIn(
                            Pattern::Var("_d0".to_string(), false),
                            Box::new(Unbox(Box::new(Var(Vec::new(), "rec_const".to_string())))),
                            Box::new(Delay(Box::new(App(
                                Box::new(Adv(Box::new(Var(Vec::new(), "_d0".to_string())))),
                                Box::new(Var(Vec::new(), "x".to_string()))
                            ))))
                        )),
                    ]))))
                ))
            )
        )
    );
}

#[test]
fn test_static_recursive() {
    let program = r"
let contains = fn v xs -> 
    match xs with {
        [] => false,
        x:xs => if v == x then true else contains v xs,
    }
";
    let test_code = crate::parser::parse_source(program.to_string()).unwrap();
    match *test_code[0].clone() {
        crate::parser::ast::PExpr::Let(pat, expr) => {
            let e = Expr::from_pexpr(*expr.clone()).unwrap();

            // If e is recursive within one time step
            match pat {
                Pattern::Var(var, _) => {
                    assert!(e.is_static_recursive(&var));
                }
                _ => (),
            };
        }
        _ => (),
    }
}

#[test]
fn reject_static_recursive() {
    let program = r"
let map = fn f (a << as) -> !#f a << @(map f !@as)
";
    let test_code = crate::parser::parse_source(program.to_string()).unwrap();
    match *test_code[0].clone() {
        crate::parser::ast::PExpr::Let(pat, expr) => {
            let e = Expr::from_pexpr(*expr.clone()).unwrap();

            // If e is recursive within one time step
            match pat {
                Pattern::Var(var, _) => {
                    assert!(!e.is_static_recursive(&var));
                }
                _ => (),
            };
        }
        _ => (),
    }
}
