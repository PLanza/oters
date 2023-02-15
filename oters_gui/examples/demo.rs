extern crate oters_gui;

fn main() {
    let config = oters_gui::WindowConfig {
        title: "Demo".to_string(),
        dimensions: (720, 480),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters_lang::run!(vec!["./oters_gui/examples/demo.otrs".to_string()], config,);
}
