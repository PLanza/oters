fn main() {
    let config = oters::WindowConfig {
        title: "My Oters App".to_string(),
        dimensions: (800, 600),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters::run!(
        vec!["./examples/loop_test/loop_test.otrs".to_string()],
        config,
    );
}
