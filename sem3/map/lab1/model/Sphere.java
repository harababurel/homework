package model;

public class Sphere implements GeometricShape {
    private double radius;

    public Sphere(double radius) {
        this.radius = radius;
    }

    @Override
    public double getVolume() {
        return 4.0/3.0 * Math.PI * this.radius * this.radius * this.radius;
    }

    @Override
    public String toString() {
        return String.format("Sphere(%.2f)", this.getVolume());
    }
}
