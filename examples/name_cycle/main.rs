#[oters::export_oters]
fn get_nth(list: Vec<String>, n: i64) -> String {
    list[n as usize].clone()
}

fn main() {
    let config = oters::WindowConfig {
        title: "My Oters App".to_string(),
        dimensions: (800, 600),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters::run!(
        vec![
            "./examples/name_cycle/data.otrs".to_string(),
            "./examples/name_cycle/main.otrs".to_string()
        ],
        config,
    );
}
