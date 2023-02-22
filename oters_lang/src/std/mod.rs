use crate as oters;
use crate::export::export_oters;

#[export_oters]
pub fn print_int(i: i64) {
    println!("{i}")
}

#[export_oters]
pub fn print_float(f: f64) {
    println!("{f}")
}

#[export_oters]
pub fn print_string(s: String) {
    println!("{s}")
}

#[export_oters]
pub fn print_bool(b: bool) {
    println!("HI{b}")
}

#[export_oters]
pub fn int_to_string(i: i64) -> String {
    i.to_string()
}

#[export_oters]
pub fn float_to_string(f: i64) -> String {
    f.to_string()
}

#[export_oters]
pub fn int_to_float(i: i64) -> f64 {
    i as f64
}

#[export_oters]
pub fn float_to_int(f: f64) -> i64 {
    f as i64
}

#[export_oters]
pub fn timestamp_millis() -> i64 {
    chrono::Utc::now().timestamp_millis()
}
