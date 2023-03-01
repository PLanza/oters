use oters_lang as oters;

use oters::export::export_oters;

use super::{Alignment, Frame, UIInstance, UIType, FRAMES};

const PADDING: u32 = 10;
#[export_oters]
pub fn create_vgroup(frame_id: i64, size: (i64, i64), align: Alignment) -> i64 {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let id = frame.elems.len();
    frame.elems.push(UIInstance {
        ty: UIType::VGroup(align),
        pos: (0, 0),
        size: (size.0 as u32, size.1 as u32),
        visible: false,
    });
    id as i64
}

#[export_oters]
pub fn draw_vgroup(frame_id: i64, grp_id: i64, elems: Vec<i64>) {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let grp = &frame.elems[grp_id as usize].clone();
    match grp {
        UIInstance {
            ty: UIType::VGroup(align),
            pos: g_pos,
            size: g_size,
            visible,
        } => {
            if !visible {
                return oters_lang::export::Value::Unit;
            }
            match align {
                Alignment::Left => draw_left(*g_pos, *g_size, frame, elems),
                Alignment::Right => draw_right(*g_pos, *g_size, frame, elems),
                _ => draw_left(*g_pos, *g_size, frame, elems),
            }
        }
        _ => panic!("Expected a vgroup"),
    }
}

fn draw_left(g_pos: (u32, u32), g_size: (u32, u32), frame: &mut Frame, elems: Vec<i64>) {
    let max_x = g_pos.0 + g_size.0 - PADDING;
    let max_y = g_pos.1 + g_size.1 - PADDING;

    let mut x = g_pos.0 + PADDING;
    let mut y = g_pos.1 + PADDING;
    let mut dy = 0;

    for e_id in elems {
        let elem = &mut frame.elems[e_id as usize];
        match elem.ty {
            UIType::Separator => {
                elem.size = (g_size.0 - 2 * PADDING, 2);
            }
            _ => (),
        }
        if !(x + elem.size.0 > max_x) {
            if !(y + elem.size.1 > max_y) {
                elem.pos = (x, y);
                elem.visible = true;
                x += elem.size.0 + PADDING;
                dy = dy.max(elem.size.1 + PADDING);
            } else {
                break;
            }
        } else {
            if !(g_pos.0 + PADDING + elem.size.0 > max_x) {
                x = g_pos.0 + PADDING;
                y += dy;
                dy = 0;
                elem.pos = (x, y);
                elem.visible = true;
                x += elem.size.0 + PADDING;
                dy = dy.max(elem.size.1 + PADDING);
            }
        }
    }
}

fn draw_right(g_pos: (u32, u32), g_size: (u32, u32), frame: &mut Frame, elems: Vec<i64>) {
    let min_x = g_pos.0 + PADDING;
    let max_y = g_pos.1 + g_size.1 - PADDING;

    let mut x = g_pos.0 + g_size.0 - PADDING;
    let mut y = g_pos.1 + PADDING;
    let mut dy = 0;

    for e_id in elems {
        let elem = &mut frame.elems[e_id as usize];
        match elem.ty {
            UIType::Separator => {
                elem.size = (g_size.0 - 2 * PADDING, 2);
            }
            _ => (),
        }
        if x > elem.size.0 && !(x - elem.size.0 < min_x) {
            if !(y + elem.size.1 > max_y) {
                elem.pos = (x - elem.size.0, y);
                elem.visible = true;
                x -= elem.size.0 + PADDING;
                dy = dy.max(elem.size.1 + PADDING);
            } else {
                break;
            }
        } else {
            if !(g_pos.0 + g_size.0 - PADDING - elem.size.0 < min_x) {
                x = g_pos.0 + g_size.0 - PADDING;
                y += dy;
                dy = 0;
                elem.pos = (x - elem.size.0, y);
                elem.visible = true;
                x -= elem.size.0 - PADDING;
                dy = dy.max(elem.size.1 + PADDING);
            }
        }
    }
}
