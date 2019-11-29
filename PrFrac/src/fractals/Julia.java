package fractals;

import processing.core.PApplet;
import processing.core.PVector;

public class Julia extends Mandelbrot {
    public Julia(PApplet p) {
        super(p);
    }
    private float angle = 0f;

    @Override
    PVector getConstant() {
        return new PVector((float) Math.cos(angle), (float) Math.sin(angle));
    }

    @Override
    void iterate () {
        angle += 0.1;
    }

    @Override
    public void draw() {
        this.generate();
        super.draw();
    }
}
