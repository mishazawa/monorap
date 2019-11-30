mod primitives;
mod renderer;

extern crate minifb;



// use crate::primitives::line::line;
use crate::renderer::setup::setup;
use crate::renderer::draw::draw;

use crate::renderer::{Renderer};

pub const WIDTH: usize = 640;
pub const HEIGHT: usize = 360;




fn draw_cb (_renderer: &mut Renderer) -> Vec<Vec<u32>> {
  let mut layers = vec![];

  layers.push(vec![0xffff0000; WIDTH * HEIGHT]);
  layers.push(vec![0xaa0000ff; WIDTH * HEIGHT]);

  layers
}


fn main() {
  let mut renderer = Renderer::new();
  setup();
  draw(&mut renderer, draw_cb);
}
