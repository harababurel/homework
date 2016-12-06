package models;
import java.io.*;

public class ReadHeapExp extends Exp implements Serializable {
    private String var_name;

    public ReadHeapExp(String var_name) {
        this.var_name = var_name;
    }

    @Override
    public int eval(MyIDictionary <String, Integer> symTable, MyIHeap heap) throws Exception {
        return heap.read_location(symTable.get(this.var_name));
    }

    @Override
    public String toString() {
        return this.var_name;
    }
}
