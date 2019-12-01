#[derive(Debug)]
pub struct TransformMatrix {
    pub rotation: f32,
    pub rotation_sin_cos: (f32, f32),
    pub translation: (i32, i32),
    pub scale: f32,
}

pub trait Transformation {
    fn rotate(&self, xy: (i32, i32)) -> (i32, i32);
    fn translate(&self, xy: (i32, i32)) -> (i32, i32);
    fn scale(&self, xy: (i32, i32)) -> (i32, i32);
}

impl TransformMatrix {
    pub fn new() -> Self {
        TransformMatrix {
            rotation: 0.0,
            rotation_sin_cos: (0., 1.),
            translation: (0, 0),
            scale: 0.0,
        }
    }



    pub fn add(&mut self, matrix: &Self) -> &mut Self {
        self.rotation = matrix.rotation;
        self.rotation_sin_cos = (self.rotation.sin(), self.rotation.cos());

        self.translation = (
            self.translation.0 + matrix.translation.0,
            self.translation.1 + matrix.translation.1,
        );

        self.scale += matrix.scale;

        self
    }
}

impl Transformation for TransformMatrix {
    fn rotate(&self, xy: (i32, i32)) -> (i32, i32) {
        let new_x = xy.0 as f32 * self.rotation_sin_cos.1 - xy.1 as f32 * self.rotation_sin_cos.0;
        let new_y = xy.0 as f32 * self.rotation_sin_cos.0 - xy.1 as f32 * self.rotation_sin_cos.1;
        (new_x as i32, new_y as i32)
    }

    fn translate(&self, xy: (i32, i32)) -> (i32, i32) {
        (self.translation.0 + xy.0, self.translation.1 + xy.1)
    }

    fn scale(&self, xy:(i32, i32)) -> (i32, i32) {
        return match self.scale {
            s if s == 0.0 => xy,
            _ => ((self.scale * xy.0 as f32) as i32, (self.scale * xy.1 as f32) as i32)
        }
    }
}
