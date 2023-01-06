extern crate oters;

use oters::export::Value;
use oters::export::{export_list, export_oters};

#[export_oters]
struct Circle {
    pos: (i64, i64),
    r: f64,
}

#[export_oters]
fn circ_area(c: Circle) -> f64 {
    c.r * c.r * 3.14159265358979
}

#[export_oters]
fn print_int(i: i64) {
    println!("{}", i);
}

#[export_oters]
fn int_sum(i1: i64, i2: i64) -> i64 {
    i1 + i2
}

#[export_oters]
fn list_sum(xs: Vec<i64>) -> i64 {
    xs.into_iter().sum()
}

#[export_oters]
fn add_one(xs: Vec<i64>) -> Vec<i64> {
    xs.into_iter().map(|i| i + 1).collect::<Vec<i64>>()
}

#[export_oters]
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
    println!(
        "{:?}",
        EXPORT_FNS.get("circ_area").unwrap().0(vec![Value::Struct(
            "Circle".to_string(),
            HashMap::from([
                (
                    "pos".to_string(),
                    Box::new(Value::Tuple(vec![
                        Box::new(Value::Int(10)),
                        Box::new(Value::Int(10))
                    ]))
                ),
                ("r".to_string(), Box::new(Value::Float(2.0)))
            ])
        )])
    )
}
