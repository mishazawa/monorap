import fractals.Julia;
import fractals.Mandelbrot;
import fractals.Tree;
import processing.core.PApplet;

import static utils.Constants.*;

public class Main extends PApplet {

    private Tree tree;
    private Mandelbrot julia;
    private Mandelbrot mbrot;

    public void settings () {
        size(WIDTH, WIDTH);
    }
    public void setup () {
        tree = new Tree(this, ITERATIONS);
        mbrot = new Mandelbrot(this);
        julia = new Julia(this);
    }
    public void draw () {
        background(BACKGROUND);
        julia.draw();
    }

    public static void main (String[] args) {
        Main object = new Main();
        PApplet.runSketch(new String[]{"Sketch"}, object);
    }
}
