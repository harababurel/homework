package models;
import java.util.*;

public class MyDictionary <K, V> implements MyIDictionary <K, V> {
    private Map <K, V> map;

    public MyDictionary() {
        this.map = new HashMap <K, V>();
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
}
