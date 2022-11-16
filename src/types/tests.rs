#[allow(unused_imports)]
use std::collections::HashMap;

#[test]
fn test_stable_context() {
    let type_decs = HashMap::new();
    use super::Type;
    use crate::exprs::{
        VarContext,
        VarTerm::{self, Var},
    };

    let context = VarContext {
        terms: vec![
            Var("name".to_owned(), Type::String),
            Var(
                "f".to_owned(),
                Type::Function(Box::new(Type::Int), Box::new(Type::Int)),
            ),
            VarTerm::Tick,
            Var("later".to_owned(), Type::Delay(Box::new(Type::Float))),
            Var(
                "boxed".to_owned(),
                Type::Stable(Box::new(Type::Fix(
                    "alpha".to_owned(),
                    Box::new(Type::Tuple(vec![
                        Box::new(Type::Int),
                        Box::new(Type::FixVar("alpha".to_owned())),
                    ])),
                ))),
            ),
        ],
        ticks: vec![2],
    };

    let stable = VarContext {
        terms: vec![
            Var("name".to_owned(), Type::String),
            Var(
                "boxed".to_owned(),
                Type::Stable(Box::new(Type::Fix(
                    "alpha".to_owned(),
                    Box::new(Type::Tuple(vec![
                        Box::new(Type::Int),
                        Box::new(Type::FixVar("alpha".to_owned())),
                    ])),
                ))),
            ),
        ],
        ticks: vec![],
    };

    assert_eq!(context.stable(&type_decs).unwrap(), stable);
}

// Test substitution
// Test Well-formedness
