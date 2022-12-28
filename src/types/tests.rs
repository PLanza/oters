#![allow(unused_imports)]

use std::cell::RefCell;
use std::rc::Rc;

#[test]
fn test_stable_context() {
    use super::Type;
    use crate::exprs::{
        VarContext,
        VarTerm::{self, Var},
    };

    let context = VarContext {
        terms: vec![
            Var(Rc::new(RefCell::new(("name".to_owned(), Type::String)))),
            Var(Rc::new(RefCell::new((
                "f".to_owned(),
                Type::Function(Box::new(Type::Int), Box::new(Type::Int)),
            )))),
            VarTerm::Tick,
            Var(Rc::new(RefCell::new((
                "later".to_owned(),
                Type::Delay(Box::new(Type::Float)),
            )))),
            Var(Rc::new(RefCell::new((
                "boxed".to_owned(),
                Type::Stable(Box::new(Type::Fix(
                    "alpha".to_owned(),
                    Box::new(Type::Tuple(vec![
                        Box::new(Type::Int),
                        Box::new(Type::FixVar("alpha".to_owned())),
                    ])),
                ))),
            )))),
        ],
        ticks: vec![2],
    };

    let stable = VarContext {
        terms: vec![
            Var(Rc::new(RefCell::new(("name".to_owned(), Type::String)))),
            Var(Rc::new(RefCell::new((
                "boxed".to_owned(),
                Type::Stable(Box::new(Type::Fix(
                    "alpha".to_owned(),
                    Box::new(Type::Tuple(vec![
                        Box::new(Type::Int),
                        Box::new(Type::FixVar("alpha".to_owned())),
                    ])),
                ))),
            )))),
        ],
        ticks: vec![],
    };

    assert_eq!(context.stable().unwrap(), stable);
}

// TODO: Test substitution
// TODO: Test Well-formedness
