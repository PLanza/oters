fn main() {
    let config = oters::WindowConfig {
        title: "UI Test".to_string(),
        dimensions: (800, 600),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters::run!(vec!["./examples/ui_test/main.otrs".to_string()], config,);
}
