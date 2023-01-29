extern crate oters;
extern crate oters_gui;

use std::collections::HashMap;

fn main() {
    let config = oters_gui::WindowConfig {
        title: "Standard Library Test".to_string(),
        dimensions: (720, 480),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters::run!(
        vec!["./oters_gui/examples/stdlib_test.otrs".to_string()],
        config,
    );
}
