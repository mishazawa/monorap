mod primitives;
mod renderer;

extern crate minifb;



// use crate::primitives::line::line;
use crate::renderer::setup::setup;
use crate::renderer::draw::draw;
use crate::renderer::color::{Color};
use crate::renderer::{Renderer};

const WIDTH: usize = 640;
const HEIGHT: usize = 360;




fn draw_cb (_renderer: &mut Renderer) -> Vec<Vec<u32>> {
  let _r = Color::new(255, 0, 0, 100);
  let _g = Color::new(0, 100, 0, 100);
  let _b = Color::new(0, 0, 255, 0);
  let _w = Color::new(0, 0, 0, 0);

  let mut layers = vec![];
  layers.push(vec![Color::blend(_r, _g).hex; WIDTH * HEIGHT]);
  layers
}


fn main() {
  let mut renderer = Renderer::new(WIDTH, HEIGHT);
  setup();
  draw(&mut renderer, draw_cb);
}
