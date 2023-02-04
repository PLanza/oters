use std::path::Path;

use anyhow::Result;
use oters::export::{export_list, ExportEnums, ExportFns, ExportStructs, PathExportFns};
use oters::parser::ast::PExpr;
use oters::types::check::ProgramChecker;
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
use std::collections::HashMap;

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
    let mut checker = ProgramChecker::new();

    // Load std and gui libraries
    let std_imports = oters::load_std_lib(&mut checker)?;
    let gui_imports = load_gui_lib(&mut checker)?;

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

    let mut export_fns = gui_imports;
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

fn get_exports(
    functions: Vec<&str>,
    structs: Vec<&str>,
    enums: Vec<&str>,
) -> (ExportFns, ExportStructs, ExportEnums) {
    let e_fs: ExportFns = functions
        .into_iter()
        .map(|s| (s.to_string(), EXPORT_FNS.get(s).cloned().unwrap()))
        .collect();
    let mut e_ss: ExportStructs = Vec::new();
    for s1 in structs {
        for s2 in EXPORT_STRUCTS.iter() {
            if s1 == s2.0 {
                e_ss.push(s2.clone());
            }
        }
    }
    let mut e_es: ExportEnums = Vec::new();
    for e1 in enums {
        for e2 in EXPORT_ENUMS.iter() {
            if e1 == e2.0 {
                e_es.push(e2.clone());
            }
        }
    }
    (e_fs, e_ss, e_es)
}

fn load_gui_file(
    checker: &mut ProgramChecker,
    pefs: &mut PathExportFns,
    code: Vec<Box<PExpr>>,
    path: Vec<String>,
    exports: (ExportFns, ExportStructs, ExportEnums),
) -> Result<()> {
    checker.type_check_program(&code, path.clone(), Some(exports.clone()))?;
    pefs.extend(
        exports
            .0
            .iter()
            .map(|(name, vals)| ((path.clone(), name.clone()), vals.clone())),
    );
    Ok(())
}

fn load_gui_lib(checker: &mut ProgramChecker) -> Result<oters::export::PathExportFns> {
    // Plus Struct Color
    let gui = vec!["frame", "attach_root"];
    let gui_widget = vec![
        "create_button",
        "draw_button",
        "create_checkbox",
        "draw_checkbox",
        "create_label",
        "draw_label",
        "create_vgroup",
        "draw_vgroup",
        "create_separator",
        "draw_separator",
        "create_textbox",
        "draw_textbox",
        "create_guimage",
        "draw_guimage",
    ];
    let gui_time = vec![
        "fps",
        "draw_fps_once",
        "time_since_start",
        "current_time",
        "timestamp_millis",
    ];
    // Plus struct Image
    let gui_image = vec![
        "img_from_file",
        "draw_img",
        "rotate_img",
        "flip_y_img",
        "new_sub_img",
        "get_pixel",
        "set_pixel",
        "get_screen_image",
    ];
    // Plus enum MouseButton
    let gui_input = vec![
        "is_key_pressed",
        "is_key_down",
        "is_key_released",
        "is_mouse_down",
        "is_mouse_pressed",
        "mouse_pos",
        "mouse_wheel",
    ];
    // Plus enum Shape
    let gui_shape = vec!["draw_shape"];
    let gui_window = vec![
        "set_bg_color",
        "window_dims",
        "resize_window",
        "set_fullscreen",
    ];

    let mut path_export_fns: PathExportFns = HashMap::new();

    let code = oters::parser::parse_source(include_str!("gui.otrs").to_string())?;
    let exports = get_exports(gui, vec!["Color"], Vec::new());
    load_gui_file(
        checker,
        &mut path_export_fns,
        code,
        vec!["gui".to_string()],
        exports,
    )?;

    let code = oters::parser::parse_source(include_str!("gui/widget.otrs").to_string())?;
    let exports = get_exports(gui_widget, Vec::new(), vec!["Alignment"]);
    load_gui_file(
        checker,
        &mut path_export_fns,
        code,
        vec!["gui".to_string(), "widget".to_string()],
        exports,
    )?;

    let code = oters::parser::parse_source(include_str!("gui/time.otrs").to_string())?;
    let exports = get_exports(gui_time, Vec::new(), Vec::new());
    load_gui_file(
        checker,
        &mut path_export_fns,
        code,
        vec!["gui".to_string(), "time".to_string()],
        exports,
    )?;

    let code = oters::parser::parse_source(include_str!("gui/image.otrs").to_string())?;
    let exports = get_exports(gui_image, vec!["Image"], Vec::new());
    load_gui_file(
        checker,
        &mut path_export_fns,
        code,
        vec!["gui".to_string(), "image".to_string()],
        exports,
    )?;

    let code = oters::parser::parse_source(include_str!("gui/input.otrs").to_string())?;
    let exports = get_exports(gui_input, Vec::new(), vec!["MouseButton"]);
    load_gui_file(
        checker,
        &mut path_export_fns,
        code,
        vec!["gui".to_string(), "input".to_string()],
        exports,
    )?;

    let code = oters::parser::parse_source(include_str!("gui/shape.otrs").to_string())?;
    let exports = get_exports(gui_shape, Vec::new(), vec!["Shape"]);
    load_gui_file(
        checker,
        &mut path_export_fns,
        code,
        vec!["gui".to_string(), "shape".to_string()],
        exports,
    )?;

    let code = oters::parser::parse_source(include_str!("gui/window.otrs").to_string())?;
    let exports = get_exports(gui_window, Vec::new(), Vec::new());
    load_gui_file(
        checker,
        &mut path_export_fns,
        code,
        vec!["gui".to_string(), "window".to_string()],
        exports,
    )?;

    Ok(path_export_fns)
}
