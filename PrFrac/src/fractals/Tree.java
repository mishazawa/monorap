package fractals;

import processing.core.PApplet;
import utils.Constants;

import java.util.ArrayList;

import static utils.Constants.BRANCH_ANGLE;
import static utils.Constants.BRANCH_LENGTH;

public class Tree implements Drawable {
    private Leaf leaf;
    private PApplet p;

    public Tree(PApplet proc, int iter) {
        this.p = proc;
        this.leaf = new Leaf(proc, iter, 0f);
    }

    public void draw () {
        p.translate(Constants.WIDTH / 2,Constants.WIDTH);
        p.rotate(p.PI);
        this.leaf.draw();
    }
}

class Leaf implements Drawable {
    private PApplet p;
    private ArrayList<Drawable> children;
    private float rotation;
    private float length;

    Leaf(PApplet proc, int length, float rotation) {
        this.p = proc;
        this.rotation = rotation;
        this.length = BRANCH_LENGTH * length;

        if (length > 0) {
            this.children = new ArrayList<>();
            this.children.add(new Leaf(this.p, length - 1, rotation + BRANCH_ANGLE));
            this.children.add(new Leaf(this.p, length - 1, rotation - BRANCH_ANGLE));
        }
    }


    public void draw() {
        p.pushStyle();
        p.stroke(255, 0, 255);
        p.pushMatrix();
        p.rotate(this.rotation);
        p.line(0, 0, 0, this.length);
        p.translate(0, this.length);

        if (this.children != null) this.children.forEach(Drawable::draw);

        p.popMatrix();
        p.popStyle();
    }
}
