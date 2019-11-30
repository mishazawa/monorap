use minifb::{ Window, WindowOptions, Key };
use color::{ SimpleColor, Color, Colored };

use crate::primitives;

pub mod color;

#[derive(Debug)]
pub struct Renderer {
  pub width: usize,
  pub height: usize,
  pub window: Window,
  pub buffer: Vec<u32>,

  pub stroke_color: SimpleColor,
  pub fill_color: SimpleColor,
  pub fill: bool,
  pub stroke: bool,
}

impl Renderer {
  pub fn new (width: usize, height: usize) -> Self {
    let vec_len = width * height;
    let window = Window::new(
      "Test - ESC to exit",
      width,
      height,
      WindowOptions::default()
    ).unwrap_or_else(|e| {
      panic!("{}", e);
    });

    Self {
      buffer: vec![0; vec_len],
      window: window,
      width,
      height,

      stroke: true,
      stroke_color: Colored::BLACK as SimpleColor,
      fill: true,
      fill_color: Colored::TRANSPARENT as SimpleColor,
    }
  }
}

pub trait Processing {
  fn setup(&mut self, f: impl Fn(&mut Self) -> ());
  fn draw(&mut self, f: impl Fn(&mut Self) -> ());
  fn stroke (&mut self, color: Color);
  fn no_stroke (&mut self, flag: bool);
  fn background (&mut self, color: Color);
  fn dot (&mut self, x: i32, y: i32);
  fn line (&mut self, x0: i32, y0: i32, x1: i32, y1: i32);
}

impl Processing for Renderer {
  fn setup (&mut self, f: impl Fn(&mut Renderer) -> ()) -> () {
    f(self);
  }

  fn draw (&mut self, draw_fn: impl Fn(&mut Renderer) -> ()) {
    while self.window.is_open() && !self.window.is_key_down(Key::Escape) {
      // make operations
      draw_fn(self);
      // draw pixels
      self.window.update_with_buffer(&self.buffer).unwrap();
    }
  }

  fn stroke (&mut self, color: Color) -> () {
    self.stroke_color = color.hex;
  }
  fn no_stroke (&mut self, flag: bool) -> () {
    self.stroke = !flag;

  }

  fn background (&mut self, color: Color) -> () {
    primitives::background(self, color);
  }

  fn dot (&mut self, x: i32, y: i32) -> () {
    if self.stroke {
      primitives::dot(self, x, y);
    }
  }

  fn line (&mut self, x0: i32, y0: i32, x1: i32, y1: i32) -> () {
    primitives::line(self, x0, y0, x1, y1);
  }

}
