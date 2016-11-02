package model;

public class Cylinder implements GeometricShape {
    private double radius, height;

    public Cylinder(double radius, double height) {
        this.radius = radius;
        this.height = height;
    }

    @Override
    public double getVolume() {
        return Math.PI * this.radius * this.radius * this.height;
    }

    @Override
    public String toString() {
        return String.format("Cylinder(%.2f)", this.getVolume());
    }

}
