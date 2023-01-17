use oters::export::export_oters;

use super::{UIInstance, UIType, FRAMES};

const PADDING: u32 = 10;

// Not yet implemented
#[export_oters]
pub enum Alignment {
    Left,
    Center,
    Right,
}

#[export_oters]
pub fn create_vgroup(frame_id: i64, size: (i64, i64)) -> i64 {
    let frame = &mut FRAMES.lock().unwrap()[frame_id as usize];
    let id = frame.elems.len();
    frame.elems.push(UIInstance {
        ty: UIType::VGroup,
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
            ty: UIType::VGroup,
            pos: g_pos,
            size: g_size,
            visible,
        } => {
            if !visible {
                return oters::export::Value::Unit;
            }

            let max_x = g_pos.0 + g_size.0 - PADDING;
            let max_y = g_pos.1 + g_size.1 - PADDING;

            let mut x = g_pos.0 + PADDING;
            let mut y = g_pos.1 + PADDING;
            let mut dy = 0;

            for e_id in elems {
                let elem = frame.elems[e_id as usize].clone();
                if !(x + elem.size.0 > max_x) {
                    if !(y + elem.size.1 > max_y) {
                        frame.elems[e_id as usize] = UIInstance {
                            ty: elem.ty,
                            pos: (x, y),
                            size: elem.size,
                            visible: true,
                        };
                        x += elem.size.0 + PADDING;
                        dy = dy.max(elem.size.1 + PADDING);
                    } else {
                        break;
                    }
                } else {
                    if !(g_pos.0 + PADDING + elem.size.0 > max_x) {
                        x = g_pos.0 + PADDING;
                        y += dy;
                        frame.elems[e_id as usize] = UIInstance {
                            ty: elem.ty,
                            pos: (x, y),
                            size: elem.size,
                            visible: true,
                        };
                    }
                }
            }
        }
        _ => panic!("Expected a vgroup"),
    }
}
