use macroquad::math::vec2;
use macroquad::ui;
use oters_lang::export::export_oters;

use super::{UIInstance, UIType, FRAMES};

#[export_oters]
pub fn create_textbox(frame_id: i64, size: (i64, i64)) -> i64 {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let id = frame.elems.len();
    frame.elems.push(UIInstance {
        ty: UIType::Textbox("".to_string()),
        pos: (0, 0),
        size: (size.0 as u32, size.1 as u32),
        visible: false,
    });
    id as i64
}

#[export_oters]
pub fn draw_textbox(frame_id: i64, tbx_id: i64) {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let tbx = &mut frame.elems[tbx_id as usize];
    match tbx {
        UIInstance {
            ty: UIType::Textbox(s),
            pos,
            size,
            visible,
        } => {
            if *visible {
                ui::widgets::Editbox::new(tbx_id as u64, vec2(size.0 as f32, size.1 as f32))
                    .position(vec2(pos.0 as f32, pos.1 as f32))
                    .multiline(true)
                    .ui(&mut ui::root_ui(), s);
            }
        }
        _ => panic!("Expected a separator"),
    }
}
