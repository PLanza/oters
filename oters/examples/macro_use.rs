extern crate oters;
extern crate oters_macro;

use oters::export::Value;
use oters::export::{export_fn, export_list};

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
    let list = vec![
        Box::new(Value::Int(1)),
        Box::new(Value::Int(2)),
        Box::new(Value::Int(3)),
    ];
    let list = Value::List(list);

    let tup = Value::Tuple(vec![
        Box::new(Value::Int(24)),
        Box::new(Value::Float(3.14)),
        Box::new(Value::String("Hello, world!".to_string())),
    ]);

    println!("{:?}", *EXPORT_FNS);

    println!(
        "{:?}",
        EXPORT_FNS.get("list_sum").unwrap().0(vec![list.clone()])
    );
    println!(
        "{:?}",
        EXPORT_FNS.get("add_one").unwrap().0(vec![list.clone()])
    );
    println!("{:?}", EXPORT_FNS.get("do_nothing").unwrap().0(vec![tup]));
}
