package models;
import java.io.*;

public class LatchStmt implements IStmt, Serializable {
    private String var;
    private Exp exp;

    public LatchStmt(String var, Exp exp) {
        this.var = var;
        this.exp = exp;
    }

    @Override
    public String toString() {
        return "latch(" + this.var + ", " + this.exp.toString() + ")";
    }

    @Override
    public synchronized PrgState execute(PrgState state) throws Exception {
        // MyIStack <IStmt> stack = state.getExeStack();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIDictionary <Integer, Integer> latchTable = state.getLatchTable();
        MyIHeap heap = state.getHeap();

        int number = this.exp.eval(symTable, heap);
        int newFreeLocation = heap.alloc();

        /* if(symTable.containsKey(id)) */
        /*     symTable.put(id, val); */
        /* else */

        latchTable.put(newFreeLocation, number);
        symTable.put(this.var, newFreeLocation);

        return null;
    }
}
