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
