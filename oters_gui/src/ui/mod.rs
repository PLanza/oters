use std::sync::Mutex;

use oters_lang as oters;

use oters::export::export_oters;

use lazy_static::lazy_static;

lazy_static! {
    pub(self) static ref FRAMES: Mutex<Vec<Frame>> = Mutex::new(Vec::new());
}

pub struct Frame {
    root: i64,
    elems: Vec<UIInstance>,
    pos: (u32, u32),
}

#[derive(Debug, Clone)]
enum UIType {
    Button,
    VGroup(Alignment),
    HGroup(Alignment),
    Checkbox(bool),
    Label,
    Separator,
    Textbox(String),
    Image,
}

#[derive(Debug, Clone)]
pub struct UIInstance {
    ty: UIType,
    pos: (u32, u32),
    size: (u32, u32),
    visible: bool,
}

#[derive(Debug, Clone, Copy)]
#[export_oters]
pub enum Alignment {
    Left,
    Right,
    Top,
    Bottom,
}

#[export_oters]
pub fn frame(pos: (i64, i64)) -> i64 {
    let frames = &mut FRAMES.lock().unwrap();
    let frame = Frame {
        root: 0,
        elems: Vec::new(),
        pos: (pos.0 as u32, pos.1 as u32),
    };
    let id = frames.len();
    frames.push(frame);
    id as i64
}

#[export_oters]
pub fn attach_root(frame_id: i64, elem_id: i64) {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    frame.root = elem_id;
    let elem = &mut frame.elems[elem_id as usize];
    elem.visible = true;
    elem.pos = frame.pos;
}

pub mod button;
pub mod checkbox;
pub mod hgroup;
pub mod image;
pub mod label;
pub mod separator;
pub mod textbox;
pub mod vgroup;
pub use button::*;
pub use checkbox::*;
pub use hgroup::*;
pub use image::*;
pub use label::*;
pub use separator::*;
pub use textbox::*;
pub use vgroup::*;
