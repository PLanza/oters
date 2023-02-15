fn main() {
    let config = oters::WindowConfig {
        title: "My Oters App".to_string(),
        dimensions: (800, 600),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters::run!(vec!["./examples/demo/demo.otrs".to_string()], config,);
}
