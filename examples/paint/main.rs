fn main() {
    let config = oters::WindowConfig {
        title: "Paint".to_string(),
        dimensions: (1080, 820),
        resizable: false,
        fullscreen: false,
        icon: None,
    };
    oters::run!(
        vec![
            "./examples/paint/colors.otrs".to_string(),
            "./examples/paint/main.otrs".to_string()
        ],
        config,
    );
}
