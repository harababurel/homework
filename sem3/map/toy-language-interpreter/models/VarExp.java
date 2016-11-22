package models;

public class VarExp extends Exp {
    private String id;

    public VarExp(String id) {
        this.id = id;
    }

    @Override
    public int eval(MyIDictionary <String, Integer> symTable, MyIHeap heap) {
        return symTable.get(this.id);
    }

    @Override
    public String toString() {
        return this.id;
    }
}
