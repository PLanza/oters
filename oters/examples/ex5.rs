extern crate oters;
extern crate oters_macro;

use oters::export_fn;

#[export_fn]
fn print_int(i: i64) {
    println!("{}", i);
}

#[export_fn]
fn int_sum(i1: i64, i2: i64) -> i64 {
    i1 + i2
}

fn main() {
    use oters::export::Value;
    print_int(vec![int_sum(vec![Value::Int(32), Value::Int(-12)])]);
}
