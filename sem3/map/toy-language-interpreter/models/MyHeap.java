package models;
import java.util.*;

public class MyHeap implements MyIHeap {
    private MyDictionary <Integer, Integer> locations;
    private int next_free_location;

    public MyHeap() {
        this.locations = new MyDictionary <Integer, Integer>();
        this.next_free_location = 1;
    }

    public Set <Map.Entry <Integer, Integer>> entrySet() {
        return this.locations.entrySet();
    }

    public void setContent(MyDictionary <Integer, Integer> content) {
        this.locations = content;
    }

    @Override
    public String toString() {
        String ret = this.locations.toString() + "\n"
            + "Next free location: " + Integer.toString(this.next_free_location) + "\n";

        return ret;
    }

    public int alloc() {
        this.locations.put(this.next_free_location, 0);
        return this.next_free_location++;
    }

    public void write_location(int location, int value) throws Exception {
        if(this.locations.containsKey(location))
            this.locations.put(location, value);
        else
            throw new Exception("Invalid memory location.");
    }

    public int read_location(int location) throws Exception {
        if(this.locations.containsKey(location))
            return this.locations.get(location);
        else
            throw new Exception("Invalid memory location.");
    }
}
