package models;
import java.util.*;


public class MyStack <T> implements MyIStack <T> {
    private Stack <T> stack;

    public MyStack() {
        this.stack = new Stack <T>();
    }

    public T pop() {
        return this.stack.pop();
    }

    public void push(T value) {
        this.stack.push(value);
    }

    public boolean isEmpty() {
        return this.stack.isEmpty();
    }
}
