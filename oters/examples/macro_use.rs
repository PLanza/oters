extern crate oters;

use std::collections::{HashMap, VecDeque};

use oters::export::Value;
use oters::{export_fn, export_list};

use lazy_static::lazy_static;

#[export_fn]
fn print_int(i: i64) {
    println!("{}", i);
}

#[export_fn]
fn int_sum(i1: i64, i2: i64) -> i64 {
    i1 + i2
}

#[export_fn]
fn list_sum(xs: Vec<i64>) -> i64 {
    xs.into_iter().sum()
}

#[export_fn]
fn add_one(xs: Vec<i64>) -> Vec<i64> {
    xs.into_iter().map(|i| i + 1).collect::<Vec<i64>>()
}

#[export_fn]
fn do_nothing(tup: (i64, f64, String)) -> (i64, f64, String) {
    tup
}

export_list!();

fn main() {
    print_int(vec![int_sum(vec![Value::Int(32), Value::Int(-12)])]);
    let deque = VecDeque::from(vec![
        Box::new(Value::Int(1)),
        Box::new(Value::Int(2)),
        Box::new(Value::Int(3)),
    ]);
    let list = Value::List(deque);

    let tup = Value::Tuple(vec![
        Box::new(Value::Int(24)),
        Box::new(Value::Float(3.14)),
        Box::new(Value::String("Hello, world!".to_string())),
    ]);

    println!(
        "{:?}",
        EXPORT_FNS.get("list_sum").unwrap().0(vec![list.clone()])
    );
    println!("{:?}", add_one(vec![list.clone()]));
    println!("{:?}", do_nothing(vec![tup]));

    println!("{:?}", *EXPORT_FNS);
}
