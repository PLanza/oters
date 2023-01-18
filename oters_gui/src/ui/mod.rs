pub mod button;
pub mod checkbox;
pub mod label;
pub mod vgroup;
pub use button::*;
pub use checkbox::*;
pub use label::*;
pub use vgroup::*;

use std::sync::Mutex;

use oters::export::export_oters;

use lazy_static::lazy_static;

// TODO: Incorporate into interpreter
lazy_static! {
    pub(self) static ref FRAMES: Mutex<Vec<Frame>> = Mutex::new(Vec::new());
}

pub struct Frame {
    root: i64,
    elems: Vec<UIInstance>,
    pos: (u32, u32),
    size: (u32, u32),
}

#[derive(Debug, Clone, Copy)]
enum UIType {
    Button,
    VGroup,
    Checkbox(bool),
    Label,
}

#[derive(Debug, Clone, Copy)]
pub struct UIInstance {
    ty: UIType,
    pos: (u32, u32),
    size: (u32, u32),
    visible: bool,
}

#[export_oters]
pub enum UIElement {
    Button(i64, String),
    Container(i64, String),
}

#[export_oters]
pub fn frame(pos: (i64, i64), size: (i64, i64)) -> i64 {
    let frames = &mut FRAMES.lock().unwrap();
    let frame = Frame {
        root: 0,
        elems: Vec::new(),
        pos: (pos.0 as u32, pos.1 as u32),
        size: (size.0 as u32, size.1 as u32),
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
}
