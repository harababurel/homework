package models;
import java.io.*;

public class AssignStmt implements IStmt, Serializable {
    private String id;
    private Exp exp;

    public AssignStmt(String id, Exp exp) {
        this.id = id;
        this.exp = exp;
    }

    @Override
    public String toString() {
        return id + " = " + exp.toString();
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        // MyIStack <IStmt> stack = state.getExeStack();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIHeap heap = state.getHeap();

        int val = this.exp.eval(symTable, heap);
        // if(symTable.containsKey(id))
        //     symTable.put(id, val);
        // else
        symTable.put(this.id, val);

        return null;
    }
}
