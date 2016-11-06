package model;

public class VarExp extends Exp {
    private String id;

    public VarExp(String id) {
        this.id = id;
    }

    @Override
    public int eval(MyIDictionary <String, Integer> symTable) {
        return symTable.lookup(this.id);
    }

    @Override
    public String toString() {
        return this.id;
    }
}
