package models;
import java.util.*;

public interface MyIHeap {
    int alloc();

    void write_location(int location, int value) throws Exception;
    int read_location(int location) throws Exception;

    Set <Map.Entry <Integer, Integer>> entrySet();
    void setContent(MyDictionary <Integer, Integer> content);
}
