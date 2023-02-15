use crate as oters;
use crate::export::export_oters;

#[export_oters]
pub fn print_int(i: i64) {
    print!("{i}")
}

#[export_oters]
pub fn print_float(f: f64) {
    print!("{f}")
}

#[export_oters]
pub fn print_string(s: String) {
    print!("{s}")
}

#[export_oters]
pub fn print_bool(b: bool) {
    print!("{b}")
}

#[export_oters]
pub fn int_to_string(i: i64) -> String {
    format!("{i}").to_string()
}

#[export_oters]
pub fn timestamp_millis() -> i64 {
    chrono::Utc::now().timestamp_millis()
}
