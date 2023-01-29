use std::path::Path;

use anyhow::Result;
use oters::export::{export_list, ExportEnums, ExportFns, ExportStructs, PathExportFns};
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

pub async fn run_loop(
    files: Vec<String>,
    exports: (ExportFns, ExportStructs, ExportEnums),
) -> Result<()> {
    let mut checker = oters::types::check::ProgramChecker::new();

    // Load std and gui libraries
    let std_imports = oters::load_std_lib(&mut checker)?;

    let gui_lib = oters::parser::parse_source(include_str!("gui.otrs").to_string())?;
    checker.type_check_program(
        &gui_lib,
        vec!["gui".to_string()],
        Some((
            EXPORT_FNS.clone(),
            EXPORT_STRUCTS.clone(),
            EXPORT_ENUMS.clone(),
        )),
    )?;

    // Link user programs
    // Files must be in order of dependency
    let file_stems: Vec<String> = files
        .iter()
        .map(|f| {
            let path = Path::new(&f);
            path.file_stem().unwrap().to_str().unwrap().to_string()
        })
        .collect();

    let mut first = true;
    for (file, stem) in files.iter().zip(&file_stems) {
        let user_program = oters::parser::parse_file(file.clone())?;
        let exports = if first {
            first = false;
            Some(exports.clone())
        } else {
            None
        };
        checker.type_check_program(&user_program, vec![stem.clone()], exports)?;
    }

    let exprs = checker.checked_exprs;

    let mut export_fns: PathExportFns = EXPORT_FNS
        .clone()
        .into_iter()
        .map(|(name, val)| ((vec!["gui".to_string()], name), val))
        .collect();

    export_fns.extend(std_imports);
    export_fns.extend(
        exports
            .0
            .into_iter()
            .map(|(name, val)| ((vec![file_stems[0].clone()], name), val))
            .collect::<PathExportFns>(),
    );
    let mut interpreter = oters::interpret::Interpreter::new(exprs, export_fns, file_stems)?;

    loop {
        macroquad::prelude::clear_background(macroquad::color::WHITE);

        interpreter.eval_step()?;
        macroquad::prelude::next_frame().await
    }
}

pub fn run(
    files: Vec<String>,
    config: WindowConfig,
    exports: (ExportFns, ExportStructs, ExportEnums),
) {
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
        if let Err(err) = run_loop(files, exports).await {
            macroquad::logging::error!("Error: {:?}", err);
        }
    });
}
