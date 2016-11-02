package model;

public class Cube implements GeometricShape {
    private double length;

    public Cube(double length) {
        this.length = length;
    }

    @Override
    public double getVolume() {
        return this.length * this.length * this.length;
    }

    @Override
    public String toString() {
        return String.format("Cube(%.2f)", this.getVolume());
    }
}
