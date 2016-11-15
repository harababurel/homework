package models;

public interface MyIStack <T> {
    T pop();
    void push(T value);
    boolean isEmpty();
}
