package models;

public interface MyIList <T> {
    boolean add(T value);
    void add(int index, T value);
    void clear();
    boolean contains(T value);
    T get(int index);
    T remove(int index);
    boolean remove(Object o);
    T set(int index, T value);
    int size();
}
