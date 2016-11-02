package repository;

import model.GeometricShape;

public class InMemRepository implements Repository {
    public int size;
    public GeometricShape[] v;

    public InMemRepository(int capacity) {
        size = 0;
        v = new GeometricShape[capacity];
    }

    @Override
    public int getSize() {
        return this.size;
    }

    @Override
    public GeometricShape getNth(int n) {
        return this.v[n];
    }


    @Override
    public void add(GeometricShape x) {
        v[size++] = x;
    }

    @Override
    public void remove(GeometricShape x) {
        for(int i=0; i<this.size; i++) {
            if(v[i] == x) {
                v[i] = v[this.size - 1];
                this.size--;
                i--;
            }
        }
    }

    public boolean exists(GeometricShape x) {
        for(GeometricShape y:v) {
            if(x == y)
                return true;
        }
        return false;
    }

    public GeometricShape[] getAll() {
        return this.v;
    }
}
