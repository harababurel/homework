package models;
import java.util.*;

public interface MyIDictionary <K, V> {
    void clear();
    boolean containsKey(K key);
    boolean containsValue(V value);
    V get(K key);
    boolean isEmpty();
    V put(K key, V value);
    V remove(K key);
    int size();
    Collection <V> values();
    Set <Map.Entry <K, V>> entrySet();
    MyIDictionary <K, V> clone();
}
