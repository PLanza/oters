#![allow(unused_imports)]

use crate::{
    parser::{ast::Pattern, span::SpPattern},
    types::Type,
};

use super::{Expr, SpExpr};
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
            SpPattern::unspanned(Pattern::Var("test".to_string(), false)),
            SpExpr::unspanned(Fn(
                SpPattern::unspanned(Pattern::Var("x".to_string(), false)),
                SpExpr::unspanned(LetIn(
                    SpPattern::unspanned(Pattern::Var("_d0".to_string(), false)),
                    SpExpr::unspanned(Delay(SpExpr::unspanned(BinOp(
                        SpExpr::unspanned(Var(Vec::new(), "x".to_string())),
                        super::BOpcode::Add,
                        SpExpr::unspanned(Int(2))
                    )))),
                    SpExpr::unspanned(Delay(SpExpr::unspanned(Adv(SpExpr::unspanned(Var(
                        Vec::new(),
                        "_d0".to_string()
                    ))))))
                ))
            ))
        )
    );
}

// Pattern variable capturing
#[test]
fn test_pattern_var_capturing() {
    // [Option::Some x, MyStruct { field: y, z, ..}] | (_, a:b) << c
    use crate::parser::ast::Pattern::*;
    let pat = SpPattern::unspanned(Or(
        SpPattern::unspanned(List(vec![
            SpPattern::unspanned(Variant(
                vec!["Option".to_string()],
                "Some".to_string(),
                Some(SpPattern::unspanned(Var("x".to_string(), false))),
            )),
            SpPattern::unspanned(Struct(
                Vec::new(),
                "MyStruct".to_string(),
                vec![
                    (
                        "field".to_string(),
                        Some(SpPattern::unspanned(Var("y".to_string(), false))),
                    ),
                    ("z".to_string(), None),
                    ("..".to_string(), None),
                ],
            )),
        ])),
        SpPattern::unspanned(Stream(
            SpPattern::unspanned(Tuple(vec![
                SpPattern::unspanned(Underscore),
                SpPattern::unspanned(Cons(
                    SpPattern::unspanned(Var("a".to_string(), false)),
                    SpPattern::unspanned(Var("b".to_string(), false)),
                )),
            ])),
            SpPattern::unspanned(Var("c".to_string(), false)),
        )),
    ));

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
            SpPattern::unspanned(Pattern::Var("const".to_string(), false)),
            SpExpr::unspanned(Fix(
                "rec_const".to_string(),
                SpExpr::unspanned(Fn(
                    SpPattern::unspanned(Pattern::Var("x".to_string(), true)),
                    SpExpr::unspanned(Into(SpExpr::unspanned(Tuple(vec![
                        SpExpr::unspanned(Var(Vec::new(), "x".to_string())),
                        SpExpr::unspanned(LetIn(
                            SpPattern::unspanned(Pattern::Var("_d0".to_string(), false)),
                            SpExpr::unspanned(Unbox(SpExpr::unspanned(Var(
                                Vec::new(),
                                "rec_const".to_string()
                            )))),
                            SpExpr::unspanned(Delay(SpExpr::unspanned(App(
                                SpExpr::unspanned(Adv(SpExpr::unspanned(Var(
                                    Vec::new(),
                                    "_d0".to_string()
                                )))),
                                SpExpr::unspanned(Var(Vec::new(), "x".to_string()))
                            ))))
                        )),
                    ]))))
                ))
            ))
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
    match *test_code[0].term.clone() {
        crate::parser::ast::PExpr::Let(pat, expr) => {
            let e = SpExpr::from_pexpr(expr.clone()).unwrap();

            // If e is recursive within one time step
            match *pat.term {
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
    match *test_code[0].term.clone() {
        crate::parser::ast::PExpr::Let(pat, expr) => {
            let e = SpExpr::from_pexpr(expr.clone()).unwrap();

            // If e is recursive within one time step
            match *pat.term {
                Pattern::Var(var, _) => {
                    assert!(!e.is_static_recursive(&var));
                }
                _ => (),
            };
        }
        _ => (),
    }
}
