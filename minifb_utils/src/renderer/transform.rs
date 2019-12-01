#[derive(Debug)]
pub struct TransformMatrix {
    pub rotation: f32,
    pub translation: (i32, i32),
    pub scale: f32,
}

trait Transformation {
    fn rotate(&self, xy: (i32, i32)) -> (i32, i32);
    fn translate(&self, xy: (i32, i32)) -> (i32, i32);
    fn scale(&self, xy: (i32, i32)) -> (i32, i32);
}

impl TransformMatrix {
    pub fn new() -> Self {
        TransformMatrix {
            rotation: 0.0,
            translation: (0, 0),
            scale: 1.0,
        }
    }

    pub fn add(&mut self, matrix: &Self) -> &mut Self {
        self.rotation += matrix.rotation;
        self.translation = (
            self.translation.0 + matrix.translation.0,
            self.translation.1 + matrix.translation.1,
        );
        self.scale += matrix.scale;
        self
    }

    pub fn apply(&self, x: i32, y: i32) -> (i32, i32) {
        self.rotate(self.scale(self.translate((x, y))))

    }
}

impl Transformation for TransformMatrix {
    fn rotate(&self, xy: (i32, i32)) -> (i32, i32) {
        xy
    }

    fn translate(&self, xy: (i32, i32)) -> (i32, i32) {
        (self.translation.0 + xy.0, self.translation.1 + xy.1)
    }
    fn scale(&self, xy:(i32, i32)) -> (i32, i32) {
        ((self.scale * xy.0 as f32) as i32, (self.scale * xy.1 as f32) as i32)
    }
}
