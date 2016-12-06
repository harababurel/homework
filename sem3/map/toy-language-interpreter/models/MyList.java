package models;
import java.util.*;
import java.io.*;

public class MyList <T> implements MyIList <T>, Serializable  {
    private List <T> l;

    public MyList() {
        this.l = new LinkedList <T>();
    }

    public boolean add(T value) {
        return this.l.add(value);
    }

    public void add(int index, T value) {
        this.l.add(index, value);
    }

    public void clear() {
        this.l.clear();
    }

    public boolean contains(T value) {
        return this.l.contains(value);
    }

    public T get(int index) {
        return this.l.get(index);
    }

    public T remove(int index) {
        return this.l.remove(index);
    }

    public boolean remove(Object o) {
        return this.l.remove(o);
    }

    public T set(int index, T value) {
        return this.l.set(index, value);
    }

    public int size() {
        return this.l.size();
    }

    @Override
    public String toString() {
        String ret = "";
        for(T entry:this.l)
            ret += entry.toString() + "\n";
        return ret;
    }

}
