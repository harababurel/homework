package controller;

import model.GeometricShape;
import repository.Repository;

public class Controller {
    Repository r;

    public Controller(Repository r) {
        this.r = r;
    }

    public void add(GeometricShape x) {
        this.r.add(x);
    }

    public void remove(GeometricShape x) {
        this.r.remove(x);
    }

    public boolean exists(GeometricShape x) {
        return this.r.exists(x);
    }

    public GeometricShape[] FilterByVolume(double threshold) {
        int solSize = 0;

        for(int i=0; i<this.r.getSize(); i++) {
            if(this.r.getNth(i).getVolume() > threshold)
                solSize++;
        }

        GeometricShape[] ret = new GeometricShape[solSize];
        int retsize = 0;

        for(int i=0; i<this.r.getSize(); i++)
            if(this.r.getNth(i).getVolume() > threshold)
                ret[retsize++] = this.r.getNth(i);

        return ret;
    }
}
