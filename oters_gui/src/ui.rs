use oters::export::export_oters;

use crate::color::Color;

use macroquad::ui;

#[export_oters]
pub enum UIElement {
    Button(String, (i64, i64), Color), // Content, size, color
}

#[export_oters]
pub fn draw_ui(elem: UIElement, pos: (i64, i64)) {
    match elem {
        UIElement::Button(content, size, color) => {
            let mut skin = ui::root_ui().default_skin();
            skin.button_style = get_button_style(color);
            ui::root_ui().push_skin(&skin);
            ui::widgets::Button::new(content)
                .position(Some((pos.0 as f32, pos.1 as f32).into()))
                .size((size.0 as f32, size.1 as f32).into())
                .ui(&mut ui::root_ui());
            ui::root_ui().pop_skin();
        }
    }
}

fn get_button_style(color: Color) -> ui::Style {
    use macroquad::color;
    let hsl = color::rgb_to_hsl(color.to_macroquad());
    let mut s_builder = ui::root_ui().style_builder().color(color.to_macroquad());
    s_builder = s_builder.color_hovered(color::hsl_to_rgb(hsl.0, hsl.1, hsl.2 - 0.15));
    s_builder = s_builder.color_clicked(color::hsl_to_rgb(hsl.0, hsl.1, hsl.2 - 0.07));
    s_builder = s_builder.text_color(if hsl.2 < 0.2 {
        color::WHITE
    } else {
        color::BLACK
    });

    s_builder.build()
}
