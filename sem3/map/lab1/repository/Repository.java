package repository;

import model.GeometricShape;

public interface Repository {
    void add(GeometricShape x);
    void remove(GeometricShape x);
    int getSize();
    boolean exists(GeometricShape x);
    GeometricShape getNth(int n);
    public GeometricShape[] getAll();
}
