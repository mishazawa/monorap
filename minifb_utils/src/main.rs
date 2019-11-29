mod primitives;
mod renderer;

extern crate minifb;

use minifb::{ Key };


use crate::primitives::line::line;
use crate::renderer::setup::setup;
use crate::renderer::draw::draw;
use crate::renderer::{Renderer};

const WIDTH: usize = 640;
const HEIGHT: usize = 360;


fn main() {

  let mut renderer = Renderer::new(WIDTH, HEIGHT);

  setup();
  draw();

  while renderer.window.is_open() && !renderer.window.is_key_down(Key::Escape) {
    for i in renderer.buffer.iter_mut() {
      line(0, 0, 1, 1);
      *i = 0; // write something more funny here!
    }
    renderer.window.update_with_buffer(&renderer.buffer).unwrap();
  }
}
