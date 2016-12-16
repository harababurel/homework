package models;
import java.util.*;
import java.io.*;

public class MyDictionary <K, V> implements MyIDictionary <K, V>, Serializable {
    private Map <K, V> map;

    public MyDictionary() {
        this.map = new HashMap <K, V>();
    }

    public MyDictionary <K, V> clone() {
        MyDictionary <K, V> clonedMap = new MyDictionary <K, V>();

        for(Map.Entry <K, V> entry:this.map.entrySet())
             clonedMap.put(entry.getKey(), entry.getValue());

        return clonedMap;
    }

    public void clear() {
        this.map.clear();
    }

    public boolean containsKey(K key) {
        return this.map.containsKey(key);
    }

    public boolean containsValue(V value) {
        return this.map.containsValue(value);
    }

    public V get(K key) {
        return this.map.get(key);
    }

    public boolean isEmpty() {
        return this.map.isEmpty();
    }

    public V put(K key, V value) {
        return this.map.put(key, value);
    }

    public V remove(K key) {
        return this.map.remove(key);
    }

    public int size() {
        return this.map.size();
    }

    public Collection <V> values() {
        return this.map.values();
    }

    public Set <Map.Entry <K, V>> entrySet() {
        return this.map.entrySet();
    }


    @Override
    public String toString() {
        String ret = "";
        for(Map.Entry <K, V> entry:this.map.entrySet()) {
            ret += entry.getKey().toString() + " -> " + entry.getValue().toString() + "\n";
        }

        return ret;
    }
}
