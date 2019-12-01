extern crate minifb;

mod color;
mod primitives;
mod renderer;
mod util;

use crate::color::Color;
use crate::renderer::{Processing, Renderer, ShapeMode};

pub const WIDTH: usize = 640;
pub const HEIGHT: usize = 360;

fn main() {
    let mut processing = Renderer::new(WIDTH, HEIGHT);

    processing.setup(|_p| {});

    let mut frame = 0;

    processing.draw(|p| {
        p.background(Color::from(0xffffffff));
        // for i in 100..200 {
        //     p.stroke(Color::from([i, i, i, i]));

        //     if i % 2 == 0 {
        //         p.no_stroke(true);
        //     } else {
        //         p.no_stroke(false);
        //     }
        //     p.dot(i as i32, i as i32);
        // }
        // p.dot(-1000, -121313);

        // p.line(0, 0, WIDTH as i32, HEIGHT as i32);
        // p.line(-10, -10, WIDTH as i32, HEIGHT as i32);

        // p.stroke(Color::from([255, 0, 0, 255]));
        // p.line(10, 10, 120, 120);
        // p.line(10, 120, 120, 10);
        // p.line(80, 200, 550, 150);

        // p.stroke(Color::from([255, 0, 255, 255]));
        // p.line(320, 0, 320, 360);

        // p.stroke(Color::from([0, 0, 0, 255]));
        // p.rect(10, 10, 20, 20);

        // p.rect(150, 10, 220, 120);

        // p.begin_shape(ShapeMode::LINES);
        // p.vertex(30, 20);
        // p.vertex(85, 20);
        // p.vertex(85, 75);
        // p.vertex(30, 75);
        // p.end_shape();

        // p.begin_shape(ShapeMode::POINTS);
        // p.vertex(10, 20);
        // p.vertex(80, 5);
        // p.vertex(95, 90);
        // p.vertex(40, 95);
        // p.end_shape();

        // p.begin_shape(ShapeMode::LINES);
        // let radius = 100.;
        // let mut angle: f32 = 0.;
        // for _ in 0..30 {
        //     let x = (radius * angle.cos()) as i32;
        //     let y = (radius * angle.sin()) as i32;
        //     p.vertex(x + frame, y + frame / 2);
        //     angle += 0.1;
        // }

        // p.end_shape();

        // frame += 1;

        // p.dot((WIDTH) as i32, (HEIGHT) as i32);

        p.push_matrix();
        for i in 0..5 {
          p.translate(10, 10);
          p.rect(0, 0, 20, 20);
          p.push_matrix();
          p.translate(100 + frame, i);
          p.rect(0, 0, 20, 20 * i);
          p.pop_matrix();
        }
        p.pop_matrix();

        frame += 1;
    });
}
