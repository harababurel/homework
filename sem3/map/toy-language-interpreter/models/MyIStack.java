package models;
import java.util.*;
import java.io.*;


public interface MyIStack <T> {
    T pop();
    void push(T value);
    boolean isEmpty();

    Stack <T> getStack();


}
