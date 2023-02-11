#![allow(unused_imports)]

use ::std::cell::RefCell;
use ::std::collections::HashMap;
use ::std::rc::Rc;

use super::check::ProgramChecker;
use super::TypeError;
use daggy::petgraph::visit::{EdgeRef, IntoEdges};
use daggy::Dag;

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

const TYPE_DECS_TEST: &str = r"
enum Option<A> {
  None,
  Some A,
}

type Stream<A> = (A, @Stream)

type Event<A> = Stream<Option<A>>

struct MyStruct<A> {
  t: (int, (), float, string, bool),
  l: [A],
  f: Event -> Stream,
  const_box: #A -> Stream<A>,
}
";

#[test]
fn type_declarations() {
    let test_code = crate::parser::parse_source(TYPE_DECS_TEST.to_string()).unwrap();
    let mut checker = ProgramChecker::new();
    checker
        .type_check_program(&test_code, Vec::new(), None)
        .unwrap();
    let types = checker.type_decs.remove_node(0.into()).unwrap();

    use super::Type::*;
    let option = Box::new(Enum(HashMap::from([
        (
            "Some".to_string(),
            Some(Box::new(GenericVar("A".to_string(), false))),
        ),
        ("None".to_string(), None),
    ])));
    let stream = Box::new(Fix(
        "rec_Stream".to_string(),
        Box::new(Tuple(vec![
            Box::new(GenericVar("A".to_string(), false)),
            Box::new(FixVar("rec_Stream".to_string())),
        ])),
    ));

    let event = Box::new(Fix(
        "rec_Stream".to_string(),
        Box::new(Tuple(vec![
            option.clone(),
            Box::new(FixVar("rec_Stream".to_string())),
        ])),
    ));

    let match_map = HashMap::from([
        ("Option", Generic(vec!["A".to_string()], option.clone())),
        ("Stream", Generic(vec!["A".to_string()], stream.clone())),
        ("Event", Generic(vec!["A".to_string()], event.clone())),
        (
            "MyStruct",
            Generic(
                vec!["A".to_string()],
                Box::new(Struct(HashMap::from([
                    (
                        "l".to_string(),
                        Box::new(List(Box::new(GenericVar("A".to_string(), false)))),
                    ),
                    (
                        "f".to_string(),
                        Box::new(Function(
                            Box::new(Generic(vec!["A".to_string()], event.clone())),
                            Box::new(Generic(vec!["A".to_string()], stream.clone())),
                        )),
                    ),
                    (
                        "t".to_string(),
                        Box::new(Tuple(vec![
                            Box::new(Int),
                            Box::new(Unit),
                            Box::new(Float),
                            Box::new(String),
                            Box::new(Bool),
                        ])),
                    ),
                    (
                        "const_box".to_string(),
                        Box::new(Function(
                            Box::new(Stable(Box::new(GenericVar("A".to_string(), false)))),
                            stream.clone(),
                        )),
                    ),
                ]))),
            ),
        ),
    ]);
    for (name, ty) in types {
        assert_eq!(ty, *match_map.get(&name.as_str()).unwrap());
    }
}

#[test]
fn load_std_lib() {
    let mut checker = ProgramChecker::new();
    let _ = crate::load_std_lib(&mut checker);
}

#[test]
fn reject_well_formed() {
    let program = r" struct MyStruct<A> { t: (int, (), float, string, bool), l: [T] }";
    let test_code = crate::parser::parse_source(program.to_string()).unwrap();
    println!("{:?}", test_code);
    match *test_code[0].term.clone() {
        crate::parser::ast::PExpr::StructDef(id, params, fields) => {
            let mut type_decs = daggy::Dag::new();
            type_decs.add_node(HashMap::new());

            let t =
                super::Type::from_structdef(id.clone(), params.clone(), fields.clone(), &type_decs);

            assert_eq!(
                t.err().unwrap().term.to_string(),
                "Type T has not been declared"
            );
        }
        _ => (),
    }
}

#[test]
fn reject_unstable_type_in_delay() {
    let program = r"
let reject = fn _ -> {
    let f = fn a b -> a + b;
    @(f 1 2)
}
";
    let test_code = crate::parser::parse_source(program.to_string()).unwrap();
    let mut checker = super::check::ProgramChecker::new();
    let e = checker.type_check_program(&test_code, Vec::new(), None);
    assert_eq!(
        e.err().unwrap().term.to_string(),
        "The variable f, cannot be accessed in the current context"
    );
}

#[test]
fn reject_adv_outside_delay() {
    let program = r"
let reject = fn _ -> {
    !@(2)
}
";
    let test_code = crate::parser::parse_source(program.to_string()).unwrap();
    let mut checker = super::check::ProgramChecker::new();
    let e = checker.type_check_program(&test_code, Vec::new(), None);
    assert_eq!(
        e.err().unwrap().term.to_string(),
        "Adv expressions must always be inside Delay expressions"
    );
}

#[test]
fn reject_unboxing_of_int() {
    let program = r"
let reject = fn _ -> {
    !#(2)
}
";
    let test_code = crate::parser::parse_source(program.to_string()).unwrap();
    let mut checker = super::check::ProgramChecker::new();
    let e = checker.type_check_program(&test_code, Vec::new(), None);
    assert_eq!(
        e.err().unwrap().term.to_string(),
        "Expected #__t1, got int instead"
    );
}

use self::oters::*;
use crate as oters;
#[crate::export::export_oters]
fn print_int(i: i64) {
    println!("{}", i);
}

crate::export::export_list!();

#[test]
fn type_check_examples() {
    let program =
        parser::parse_source(include_str!("../../examples/ex2.otrs").to_string()).unwrap();
    let mut checker = ProgramChecker::new();

    checker
        .type_check_program(
            &program,
            vec!["ex2".to_string()],
            Some((
                EXPORT_FNS.clone(),
                EXPORT_STRUCTS.clone(),
                EXPORT_ENUMS.clone(),
            )),
        )
        .unwrap();

    let program =
        parser::parse_source(include_str!("../../examples/ex1.otrs").to_string()).unwrap();
    checker
        .type_check_program(
            &program,
            vec!["ex1".to_string()],
            Some((
                EXPORT_FNS.clone(),
                EXPORT_STRUCTS.clone(),
                EXPORT_ENUMS.clone(),
            )),
        )
        .unwrap();
}
