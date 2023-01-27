extern crate oters_gui;

fn main() {
    let config = oters_gui::WindowConfig {
        title: "UI Test".to_string(),
        dimensions: (720, 480),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters_gui::run(
        vec!["./oters_gui/examples/ui_test.otrs".to_string()],
        config,
    );
}
