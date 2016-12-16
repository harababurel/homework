package models;
import java.io.*;

public class WriteHeapStmt implements IStmt, Serializable {
    private String var_name;
    private Exp exp;

    public WriteHeapStmt(String var_name, Exp exp) {
        this.var_name = var_name;
        this.exp = exp;
    }

    @Override
    public String toString() {
        return "*" + var_name + " = " + exp.toString();
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        // MyIStack <IStmt> stack = state.getExeStack();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIHeap heap = state.getHeap();

        int location = symTable.get(var_name);
        int val = this.exp.eval(symTable, heap);

        heap.write_location(location, val);
        return null;
    }
}
