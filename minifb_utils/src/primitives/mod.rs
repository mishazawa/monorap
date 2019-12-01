use crate::renderer::{ Renderer, Processing, ShapeMode};
use crate::renderer::color::Color;
use crate::util;

pub fn dot (renderer: &mut Renderer, x: i32, y: i32) -> () {
  match util::coords_to_index(x, y, renderer.width, renderer.height) {
    Some(index) => {
      renderer.buffer[index] = renderer.stroke_color;
    },
    None => (),
  }
}

pub fn line (renderer: &mut Renderer, mut x0: i32, mut y0: i32, mut x1: i32, mut y1: i32) {
  let color = Color::from(renderer.stroke_color);
  let steep = (y1 - y0).abs() > (x1 - x0).abs();

  let mut temp;
  if steep {
    temp = x0;
    x0 = y0;
    y0 = temp;

    temp = x1;
    x1 = y1;
    y1 = temp;
  }

  if x0 > x1 {
    temp = x0;
    x0 = x1;
    x1 = temp;
    temp = y0;
    y0 = y1;
    y1 = temp;
  }


  let from = (x0, y0);
  let to = (x1, y1);

  let dx = (to.0 - from.0) as f32;
  let dy = (to.1 - from.1) as f32;

  let gradient = match dx {
    dx if dx == 0.0 => 1.,
    _ => dy / dx
  };

  let x_pixel_1 = from.0;
  let x_pixel_2 = to.0;
  let mut intersect_y = from.1 as f32;

  for x in x_pixel_1..x_pixel_2 {
    let (dot_x, dot_y) = match steep {
      true => {
        (intersect_y.round() as i32, x)
      },
      false => {
        (x, intersect_y.round() as i32)
      }
    };

    renderer.stroke(Color::from([
      color.r,
      color.g,
      color.b,
      (color.a as f32 * (1. - intersect_y.fract())) as u8
    ]));
    dot(renderer, dot_x, dot_y);

    // TODO: fix proper brightness

    // renderer.stroke(Color::from([
    //   color.r,
    //   color.g,
    //   color.b,
    //   (color.a as f32 * intersect_y.fract()) as u8
    // ]));
    // dot(renderer, dot_x - 1, dot_y);

    intersect_y += gradient;
  }
  renderer.stroke(color);
}

pub fn background (renderer: &mut Renderer, color: Color) -> () {
  for pixel in renderer.buffer.iter_mut() {
    *pixel = color.hex;
  }
}

#[allow(unused_variables, dead_code)]
pub fn ellipse (renderer: &mut Renderer, x: i32, y: i32, w: i32, h: i32) {
  unimplemented!();
}

pub fn rect (renderer: &mut Renderer, x: i32, y: i32, w: i32, h: i32) {
  quad(renderer, x, y, x+w, y, x+w, y+h, x, y+h);
}

fn quad (renderer: &mut Renderer,
         x1: i32, y1: i32, x2: i32, y2: i32,
         x3: i32, y3: i32, x4: i32, y4: i32) -> () {
  renderer.begin_shape(renderer.shape_type.clone());
  renderer.vertex(x1, y1);
  renderer.vertex(x2, y2);
  renderer.vertex(x3, y3);
  renderer.vertex(x4, y4);
  renderer.end_shape();
}

#[allow(unused_variables, dead_code)]
fn fill (renderer: &mut Renderer) -> () {
  unimplemented!();
}


pub fn draw_shape (renderer: &mut Renderer) -> () {
  let shape = renderer.shape_to_draw.clone();
  for (i, _)in shape.iter().enumerate().step_by(2) {
    let begin_x = shape.get(i);
    let begin_y = shape.get(i + 1);
    let end_x = shape.get(i + 2);
    let end_y = shape.get(i + 3);

    match (begin_x, begin_y, end_x, end_y) {
      (Some(a), Some(b), Some(c), Some(d))  => {
        match renderer.shape_type {
          ShapeMode::POINTS => {
            dot(renderer, *a, *b);
          },
          ShapeMode::LINES => {
            line(renderer, *a, *b, *c, *d);
          },
        }
      },
      (Some(a), Some(b), None, None) => {
        match renderer.shape_type {
          ShapeMode::POINTS => {
            dot(renderer, *a, *b);
          },
          ShapeMode::LINES => {
            line(renderer, *a, *b, shape[0], shape[1]);
          },
        }
      },
      _ => {
        println!("Wrong shape: {:?}", shape);
      }
    }
  }
  renderer.shape_to_draw.clear();
}
