use minifb::{ Window, WindowOptions, Key };
use color::{ SimpleColor, Color, Colored };

use crate::primitives;

pub mod color;


#[derive(Debug, Clone, Copy)]
pub enum ShapeMode {
  POINTS,
  LINES,
}

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

  pub shape_type: ShapeMode,
  pub shape_to_draw: Vec<i32>,

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

      shape_type: ShapeMode::LINES,
      shape_to_draw: vec![],
    }
  }
}

pub trait Processing {
  fn setup(&mut self, f: impl FnMut(&mut Self) -> ());
  fn draw(&mut self, f: impl FnMut(&mut Self) -> ());
  fn stroke (&mut self, color: Color);
  fn no_stroke (&mut self, flag: bool);
  fn fill (&mut self, color: Color);
  fn no_fill (&mut self, flag: bool);
  fn background (&mut self, color: Color);
  fn dot (&mut self, x: i32, y: i32);
  fn line (&mut self, x0: i32, y0: i32, x1: i32, y1: i32);
  fn rect (&mut self, x: i32, y: i32, w: i32, h: i32);
  fn begin_shape(&mut self, shape_type: ShapeMode);
  fn vertex(&mut self, x: i32, y: i32);
  fn end_shape(&mut self);
}

impl Processing for Renderer {
  fn setup (&mut self, mut f: impl FnMut(&mut Renderer) -> ()) -> () {
    f(self);
  }

  fn draw (&mut self, mut draw_fn: impl FnMut(&mut Renderer) -> ()) {
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

  fn fill (&mut self, color: Color) -> () {
    self.fill_color = color.hex;
  }

  fn no_fill (&mut self, flag: bool) -> () {
    self.fill = !flag;
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

  fn rect (&mut self, x: i32, y: i32, w: i32, h: i32) -> () {
    primitives::rect(self, x, y, w, h);
  }

  fn begin_shape(&mut self, shape_type: ShapeMode) -> () {
    self.shape_type = shape_type;
    if !self.shape_to_draw.is_empty() {
      println!("Warn: Shape is not empty.");
      self.shape_to_draw.clear();
    }
  }
  fn vertex(&mut self, x: i32, y: i32) -> () {
    self.shape_to_draw.push(x);
    self.shape_to_draw.push(y);
  }
  fn end_shape(&mut self) -> () {
    primitives::draw_shape(self);
  }
}
