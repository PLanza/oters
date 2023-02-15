# Oters

Oters (**O**xidized **Te**mporal **R**eactive **S**treams) is a functional reactive programming language designed for intuitively building GUIs based on [Patrick Bahr's type system](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/modal-frp-for-all-functional-reactive-programming-without-space-leaks-in-haskell/9BE20E8D61E9B74811CF3CF97B5D10C7). 

Documentation for the language can be found [here](https://planza.github.io/Oters).



---

### Features

* <u>Type Inference</u> - Types are fully inferred such that you never have to explicitly write a type.
* <u>Interpreted</u> - While the language is interpreted, static type checking ensures that your program is safe before running.
* <u>Imports Rust Code</u> - Can import Rust functions into your Oters code with a single macro.
* <u>No space or time leakages</u> - The size and computation of the program stays constant after initiation.



---

### Dependencies

Oters relies on the [macroquad](https://github.com/not-fl3/macroquad) crate for its GUI functionality. This crate requires certain system dependencies. Specifically on Linux:

```
# ubuntu system dependencies
apt install pkg-config libx11-dev libxi-dev libgl1-mesa-dev libasound2-dev

# fedora system dependencies
dnf install libX11-devel libXi-devel mesa-libGL-devel alsa-lib-devel

# arch linux system dependencies
 pacman -S pkg-config libx11 libxi mesa-libgl alsa-lib
```



---

### Usage

Oters works as a Rust library and so must be included into your Rust project like any other dependency:

```toml
[dependencies]
oters = "0.1.2"
```

Running an Oters file requires a single macro call in your main function:

```rust
fn main() {
    let config = oters::WindowConfig {
        title: "My Oters App".to_string(),
        dimensions: (800, 600),
        resizable: true,
        fullscreen: false,
        icon: None,
    };
    oters::run!(vec!["./examples/demo/demo.otrs".to_string()], config,);
}
```

Note that files passed to the `run!` macro must be in order of dependency. So if you have two files `main.otrs` and `logic.otrs`, and the former relies on the latter, then `logic.otrs` must come before `main.otrs` in the `Vec`.

And importing a Rust function into Oters, is also done through a single macro call:

```rust
#[export_oters]
fn print_message(s: String) {
    println!("This message is being printed from a Rust function: \n{s}");
}
// Can now call the function print_message from your Oters file
```



---

### Examples

A token example taken from `examples/demo/demo.otrs`:

```
use std::stream::const
use std::stream::head
use gui::widget::*
use gui::color::*
use gui::shape::*

let ui = gui::frame ((100,50), (250, 100))

let (btn_id, btn_stream) = button ui (100, 50) (const "Click me!")

let counter = {
  let aux = fn n -> {
    let delta = if head btn_stream then 1 else 0;
    (n + delta) << @(aux (n + delta))
  };
  aux 0
}
let counter_string = std::int_to_string (head counter) << @counter_string

let (lab_id, lab_stream) = label ui (100, 50) counter_string 

let (grp_id, grp_stream) = hgroup 
  ui 
  (250,100) 
  (const [btn_id, lab_id]) 
  (Alignment::Top)

let _ = gui::attach_root (ui, grp_id)

let circle_color = 
  (if head counter % 3 == 0 then 
    red
  else if head counter %3 == 1 then
    green
  else 
    blue) << @circle_color

let circle = (draw_shape (Shape::Circle((200,200), 30, head circle_color))) << @circle
```



