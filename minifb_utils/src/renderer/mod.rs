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
  pub layers_stack: Vec<Vec<SimpleColor>>
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
      layers_stack: vec![],
    }
  }

  fn apply_layers (&self) -> Vec<SimpleColor> {
    return self.layers_stack.iter().fold(vec![Colored::TRANSPARENT as SimpleColor; self.width * self.height], |mut acc, layer| {
      for (i, color) in acc.iter_mut().enumerate() {
        *color = Color::blend_color(Color::from(*color), Color::from(layer[i]));
      }
      acc
    });
  }

  fn clear_layers (&mut self) -> () {
    self.layers_stack.clear();
  }

  pub fn push_layer (&mut self, layer_fn: impl Fn(&mut Vec<SimpleColor>) -> Vec<SimpleColor>) {
    let mut new_layer = vec![Colored::TRANSPARENT as SimpleColor; self.width * self.height];
    self.layers_stack.push(layer_fn(&mut new_layer));
  }

  pub fn draw (&mut self, draw_fn: impl Fn(&mut Renderer) -> ()) {
    while self.window.is_open() && !self.window.is_key_down(Key::Escape) {
      // make operations
      draw_fn(self);

      // draw pixels
      self.window.update_with_buffer(&self.apply_layers()).unwrap();
      // clear layers
      self.clear_layers();
    }
  }

  pub fn setup (&mut self, f: impl Fn(&mut Renderer) -> ()) -> () {
    f(self);
  }

  pub fn background (&mut self, color: Color) -> () {
    primitives::background(self, color);
  }

  pub fn dot (&mut self, x: i32, y: i32, color: Color) -> () {
    primitives::dot(self, x, y, color);
  }

  pub fn line (&self, x: i32, y: i32, x1: i32, y1: i32) -> () {
    // primitives::line(&self, x, y, x1, y1);
  }
}

