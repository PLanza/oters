use oters_lang::export::export_oters;

#[export_oters]
pub fn is_key_pressed(key: String) -> bool {
    macroquad::input::is_key_pressed(str_to_keycode(key))
}

#[export_oters]
pub fn is_key_down(key: String) -> bool {
    macroquad::input::is_key_down(str_to_keycode(key))
}

#[export_oters]
pub fn is_key_released(key: String) -> bool {
    macroquad::input::is_key_released(str_to_keycode(key))
}

#[export_oters]
pub enum MouseButton {
    Right,
    Left,
    Middle,
}

#[export_oters]
pub fn is_mouse_down(button: MouseButton) -> bool {
    use macroquad::input::is_mouse_button_down;
    match button {
        MouseButton::Right => is_mouse_button_down(macroquad::input::MouseButton::Right),
        MouseButton::Left => is_mouse_button_down(macroquad::input::MouseButton::Left),
        MouseButton::Middle => is_mouse_button_down(macroquad::input::MouseButton::Middle),
    }
}

#[export_oters]
pub fn is_mouse_pressed(button: MouseButton) -> bool {
    use macroquad::input::is_mouse_button_pressed;
    match button {
        MouseButton::Right => is_mouse_button_pressed(macroquad::input::MouseButton::Right),
        MouseButton::Left => is_mouse_button_pressed(macroquad::input::MouseButton::Left),
        MouseButton::Middle => is_mouse_button_pressed(macroquad::input::MouseButton::Middle),
    }
}

#[export_oters]
pub fn mouse_pos() -> (i64, i64) {
    let pos = macroquad::input::mouse_position();
    (pos.0 as i64, pos.1 as i64)
}

#[export_oters]
pub fn mouse_wheel() -> (f64, f64) {
    let pos = macroquad::input::mouse_wheel();
    (pos.0 as f64, pos.1 as f64)
}

// Awfully inefficient, should be redone
fn str_to_keycode(s: String) -> macroquad::input::KeyCode {
    match s.as_str() {
        "Space" => macroquad::input::KeyCode::Space,
        "Apostrophe" => macroquad::input::KeyCode::Apostrophe,
        "Comma" => macroquad::input::KeyCode::Comma,
        "Minus" => macroquad::input::KeyCode::Minus,
        "Period" => macroquad::input::KeyCode::Period,
        "Slash" => macroquad::input::KeyCode::Slash,
        "Key0" => macroquad::input::KeyCode::Key0,
        "Key1" => macroquad::input::KeyCode::Key1,
        "Key2" => macroquad::input::KeyCode::Key2,
        "Key3" => macroquad::input::KeyCode::Key3,
        "Key4" => macroquad::input::KeyCode::Key4,
        "Key5" => macroquad::input::KeyCode::Key5,
        "Key6" => macroquad::input::KeyCode::Key6,
        "Key7" => macroquad::input::KeyCode::Key7,
        "Key8" => macroquad::input::KeyCode::Key8,
        "Key9" => macroquad::input::KeyCode::Key9,
        "Semicolon" => macroquad::input::KeyCode::Semicolon,
        "Equal" => macroquad::input::KeyCode::Equal,
        "A" => macroquad::input::KeyCode::A,
        "B" => macroquad::input::KeyCode::B,
        "C" => macroquad::input::KeyCode::C,
        "D" => macroquad::input::KeyCode::D,
        "E" => macroquad::input::KeyCode::E,
        "F" => macroquad::input::KeyCode::F,
        "G" => macroquad::input::KeyCode::G,
        "H" => macroquad::input::KeyCode::H,
        "I" => macroquad::input::KeyCode::I,
        "J" => macroquad::input::KeyCode::J,
        "K" => macroquad::input::KeyCode::K,
        "L" => macroquad::input::KeyCode::L,
        "M" => macroquad::input::KeyCode::M,
        "N" => macroquad::input::KeyCode::N,
        "O" => macroquad::input::KeyCode::O,
        "P" => macroquad::input::KeyCode::P,
        "Q" => macroquad::input::KeyCode::Q,
        "R" => macroquad::input::KeyCode::R,
        "S" => macroquad::input::KeyCode::S,
        "T" => macroquad::input::KeyCode::T,
        "U" => macroquad::input::KeyCode::U,
        "V" => macroquad::input::KeyCode::V,
        "W" => macroquad::input::KeyCode::W,
        "X" => macroquad::input::KeyCode::X,
        "Y" => macroquad::input::KeyCode::Y,
        "Z" => macroquad::input::KeyCode::Z,
        "LeftBracket" => macroquad::input::KeyCode::LeftBracket,
        "Backslash" => macroquad::input::KeyCode::Backslash,
        "RightBracket" => macroquad::input::KeyCode::RightBracket,
        "GraveAccent" => macroquad::input::KeyCode::GraveAccent,
        "World1" => macroquad::input::KeyCode::World1,
        "World2" => macroquad::input::KeyCode::World2,
        "Escape" => macroquad::input::KeyCode::Escape,
        "Enter" => macroquad::input::KeyCode::Enter,
        "Tab" => macroquad::input::KeyCode::Tab,
        "Backspace" => macroquad::input::KeyCode::Backspace,
        "Insert" => macroquad::input::KeyCode::Insert,
        "Delete" => macroquad::input::KeyCode::Delete,
        "Right" => macroquad::input::KeyCode::Right,
        "Left" => macroquad::input::KeyCode::Left,
        "Down" => macroquad::input::KeyCode::Down,
        "Up" => macroquad::input::KeyCode::Up,
        "PageUp" => macroquad::input::KeyCode::PageUp,
        "PageDown" => macroquad::input::KeyCode::PageDown,
        "Home" => macroquad::input::KeyCode::Home,
        "End" => macroquad::input::KeyCode::End,
        "CapsLock" => macroquad::input::KeyCode::CapsLock,
        "ScrollLock" => macroquad::input::KeyCode::ScrollLock,
        "NumLock" => macroquad::input::KeyCode::NumLock,
        "PrintScreen" => macroquad::input::KeyCode::PrintScreen,
        "Pause" => macroquad::input::KeyCode::Pause,
        "F1" => macroquad::input::KeyCode::F1,
        "F2" => macroquad::input::KeyCode::F2,
        "F3" => macroquad::input::KeyCode::F3,
        "F4" => macroquad::input::KeyCode::F4,
        "F5" => macroquad::input::KeyCode::F5,
        "F6" => macroquad::input::KeyCode::F6,
        "F7" => macroquad::input::KeyCode::F7,
        "F8" => macroquad::input::KeyCode::F8,
        "F9" => macroquad::input::KeyCode::F9,
        "F10" => macroquad::input::KeyCode::F10,
        "F11" => macroquad::input::KeyCode::F11,
        "F12" => macroquad::input::KeyCode::F12,
        "F13" => macroquad::input::KeyCode::F13,
        "F14" => macroquad::input::KeyCode::F14,
        "F15" => macroquad::input::KeyCode::F15,
        "F16" => macroquad::input::KeyCode::F16,
        "F17" => macroquad::input::KeyCode::F17,
        "F18" => macroquad::input::KeyCode::F18,
        "F19" => macroquad::input::KeyCode::F19,
        "F20" => macroquad::input::KeyCode::F20,
        "F21" => macroquad::input::KeyCode::F21,
        "F22" => macroquad::input::KeyCode::F22,
        "F23" => macroquad::input::KeyCode::F23,
        "F24" => macroquad::input::KeyCode::F24,
        "F25" => macroquad::input::KeyCode::F25,
        "Kp0" => macroquad::input::KeyCode::Kp0,
        "Kp1" => macroquad::input::KeyCode::Kp1,
        "Kp2" => macroquad::input::KeyCode::Kp2,
        "Kp3" => macroquad::input::KeyCode::Kp3,
        "Kp4" => macroquad::input::KeyCode::Kp4,
        "Kp5" => macroquad::input::KeyCode::Kp5,
        "Kp6" => macroquad::input::KeyCode::Kp6,
        "Kp7" => macroquad::input::KeyCode::Kp7,
        "Kp8" => macroquad::input::KeyCode::Kp8,
        "Kp9" => macroquad::input::KeyCode::Kp9,
        "KpDecimal" => macroquad::input::KeyCode::KpDecimal,
        "KpDivide" => macroquad::input::KeyCode::KpDivide,
        "KpMultiply" => macroquad::input::KeyCode::KpMultiply,
        "KpSubtract" => macroquad::input::KeyCode::KpSubtract,
        "KpAdd" => macroquad::input::KeyCode::KpAdd,
        "KpEnter" => macroquad::input::KeyCode::KpEnter,
        "KpEqual" => macroquad::input::KeyCode::KpEqual,
        "LeftShift" => macroquad::input::KeyCode::LeftShift,
        "LeftControl" => macroquad::input::KeyCode::LeftControl,
        "LeftAlt" => macroquad::input::KeyCode::LeftAlt,
        "LeftSuper" => macroquad::input::KeyCode::LeftSuper,
        "RightShift" => macroquad::input::KeyCode::RightShift,
        "RightControl" => macroquad::input::KeyCode::RightControl,
        "RightAlt" => macroquad::input::KeyCode::RightAlt,
        "RightSuper" => macroquad::input::KeyCode::RightSuper,
        "Menu" => macroquad::input::KeyCode::Menu,
        _ => macroquad::input::KeyCode::Unknown,
    }
}
