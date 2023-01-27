use anyhow::Result;
use oters::export::export_list;
pub mod color;
pub mod image;
pub mod input;
pub mod shapes;
pub mod time;
pub mod ui;
pub mod window;

use crate::color::*;
use crate::image::*;
use crate::input::*;
use crate::shapes::*;
use crate::time::*;
use crate::ui::*;
use crate::window::*;

pub use macroquad::miniquad::conf::Icon;

export_list!();

pub struct WindowConfig {
    pub title: String,
    pub dimensions: (u32, u32),
    pub resizable: bool,
    pub fullscreen: bool,
    pub icon: Option<Icon>,
}

pub async fn run_loop(files: Vec<String>) -> Result<()> {
    let mut checker = oters::types::check::ProgramChecker::new((
        EXPORT_FNS.clone(),
        EXPORT_STRUCTS.clone(),
        EXPORT_ENUMS.clone(),
    ));

    // Load std and gui libraries
    oters::load_std_lib(&mut checker)?;
    let gui_lib = oters::parser::parse_source(include_str!("gui.otrs").to_string())?;
    checker.type_check_program(&gui_lib, vec!["gui".to_string()])?;

    // Link user Rust code

    // Link user programs
    // Files must be in order of dependency
    for file in files {
        let user_program = oters::parser::parse_file(file)?;
        checker.type_check_program(&user_program)?;
    }

    let exprs = checker.get_checked_exprs();

    let mut interpreter = oters::interpret::Interpreter::new(exprs, EXPORT_FNS.clone())?;

    loop {
        macroquad::prelude::clear_background(macroquad::color::WHITE);

        interpreter.eval_step()?;
        macroquad::prelude::next_frame().await
    }
}

pub fn run(files: Vec<String>, config: WindowConfig) {
    let conf = macroquad::miniquad::conf::Conf {
        window_title: config.title,
        sample_count: 4,
        window_width: config.dimensions.0 as i32,
        window_height: config.dimensions.1 as i32,
        window_resizable: config.resizable,
        fullscreen: config.fullscreen,
        icon: config.icon,
        ..Default::default()
    };
    macroquad::Window::from_config(conf, async {
        if let Err(err) = run_loop(files).await {
            macroquad::logging::error!("Error: {:?}", err);
        }
    });
}
