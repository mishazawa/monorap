package fractals;

import processing.core.PApplet;
import processing.core.PImage;
import processing.core.PVector;

import static processing.core.PConstants.HSB;
import static utils.Constants.*;

public class Mandelbrot implements Drawable {

    private PApplet p;
    private PImage img;

    private float x_min = 0f - RANGE;
    private float y_min = 0f - RANGE;

    private float x_max = 0f + RANGE;
    private float y_max = 0f + RANGE;

    private PVector re_im;

    public Mandelbrot(PApplet proc) {
        this.p = proc;
        this.img = this.p.createImage(WIDTH, WIDTH, HSB);
        this.generate();
    }

    public void draw() {
        p.colorMode(HSB);
        p.image(img, 0,0);
    }

    PVector getConstant () {
        return re_im;
    }

    void setConstant (float re, float im) {
        this.re_im = new PVector(re, im);
    }

    void iterate () {

    }

    void generate () {
        img.loadPixels();

        for (int i = 0; i < img.pixels.length; i++) {
            float real = PApplet.map((i % WIDTH), 0f, WIDTH, x_min, x_max);
            float imag = PApplet.map((i / WIDTH), 0f, WIDTH, y_min, y_max);

            this.setConstant(real, imag);

            int n = 0;
            while (n < FRACTAL_DEPTH) {
                float re = real * real - imag * imag;
                float im = 2f * real * imag;

                PVector c = this.getConstant();

                real = re + c.x;
                imag = im + c.y;

                if (Math.abs(real + imag) > GENERATION_INFINITY) {
                    break; // Leave when achieve infinity
                }
                n += 1;
            }
            img.pixels[i] = fill(n);
        }
        img.updatePixels();
        this.iterate();
    }
    private int fill (int n) {
        if (FRACTAL_DEPTH == n) {
            return this.p.color(32, 255, 0);
        } else {
            return this.p.color(n * 32 % 255, 255, 200);
        }
    }
}
