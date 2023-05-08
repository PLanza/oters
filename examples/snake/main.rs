extern crate rand;
use rand::Rng;

#[oters::export_oters]
fn new_apple() -> (i64, i64) {
    let mut rng = rand::thread_rng();
    let x = rng.gen_range(0..20);
    let y = rng.gen_range(0..20);
    (30 + x * 20, 30 + y * 20)
}

#[oters::export_oters]
fn pop_last(tail: Vec<(i64, i64)>) -> Vec<(i64, i64)> {
    let mut clone = tail.clone();
    if clone.len() > 0 {
        clone.remove(tail.len() - 1);
    }
    clone
}

#[oters::export_oters]
fn contains(head: (i64, i64), tail: Vec<(i64, i64)>) -> bool {
    tail.contains(&head)
}

fn main() {
    let config = oters::WindowConfig {
        title: "Snake".to_string(),
        dimensions: (460, 520),
        resizable: false,
        fullscreen: false,
        icon: None,
    };
    oters::run!(vec!["./examples/snake/main.otrs".to_string()], config,);
}
