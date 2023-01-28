extern crate oters;
extern crate oters_gui;

use oters::export::{export_list, export_oters};

#[export_oters]
fn print_int(i: i64) {
    println!("{}", i);
}

export_list!();

fn main() {
    let config = oters_gui::WindowConfig {
        title: "Standard Library Test".to_string(),
        dimensions: (720, 480),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters_gui::run(
        vec!["./oters_gui/examples/stdlib_test.otrs".to_string()],
        config,
        (
            EXPORT_FNS.clone(),
            EXPORT_STRUCTS.clone(),
            EXPORT_ENUMS.clone(),
        ),
    );
}
