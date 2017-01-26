package models;
import java.io.*;
import java.util.*;

public class AwaitStmt implements IStmt, Serializable {
    private String var;

    public AwaitStmt(String var) {
        this.var = var;
    }

    @Override
    public String toString() {
        return "await(" + this.var + ")";
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        MyIStack <IStmt> exeStack = state.getExeStack();
        /* List <Integer> stdout = state.getStdout(); */
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIDictionary <Integer, Integer> latchTable = state.getLatchTable();
        MyIHeap heap = state.getHeap();

        //stdout.add(this.exp.eval(symTable, heap));


        if(!symTable.containsKey(this.var))
            throw new Exception("Await error: variable name is not in the symbol table");

        int value = symTable.get(this.var);
        if(!latchTable.containsKey(value))
            throw new Exception("Await error: found index is not in the latch table");

        if(latchTable.get(value) != 0) {
            exeStack.push(this);
        }

        return null;
    }
 }
