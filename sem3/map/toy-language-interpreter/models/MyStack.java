package models;
import java.util.*;
import java.io.*;


public class MyStack <T> implements MyIStack <T>, Serializable {
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

    @Override
    public String toString() {
        String ret = "";
        for(T x:this.stack) {
            ret += x.toString() + "\n";
        }
        return ret;
    }
}
