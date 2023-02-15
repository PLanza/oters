extern crate oters_gui;

use oters_lang as oters;
use oters_lang::export::{export_list, export_oters};

#[export_oters]
fn print_int_tuple(tup: (i64, i64)) {
    println!("({}, {})", tup.0, tup.1)
}

#[export_oters]
fn print_int(i: i64) {
    println!("{}", i)
}

export_list!();

fn main() {
    let config = oters_gui::WindowConfig {
        title: "Lib Test".to_string(),
        dimensions: (720, 480),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters_lang::run!(
        vec!["./oters_gui/examples/lib_test.otrs".to_string()],
        config,
    );
}
