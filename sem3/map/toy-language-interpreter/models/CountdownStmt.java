package models;
import java.io.*;
import java.util.*;

public class CountdownStmt implements IStmt, Serializable {
    private String var;

    public CountdownStmt(String var) {
        this.var = var;
    }

    @Override
    public String toString() {
        return "countdown(" + this.var + ")";
    }

    @Override
    public synchronized PrgState execute(PrgState state) throws Exception {
        MyIStack <IStmt> exeStack = state.getExeStack();
        List <Integer> stdout = state.getStdout();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIDictionary <Integer, Integer> latchTable = state.getLatchTable();
        MyIHeap heap = state.getHeap();

        //stdout.add(this.exp.eval(symTable, heap));


        if(!symTable.containsKey(this.var))
            throw new Exception("Countdown error: variable name is not in the symbol table");

        int value = symTable.get(this.var);

        if(!latchTable.containsKey(value))
            System.out.println("Countdown: found index is not in the latch table; do nothing.");

        else if(latchTable.get(value) > 0) {
            latchTable.put(value, latchTable.get(value) - 1);

            // might be evil
            /* symTable.put(this.var, symTable.get(this.var) - 1); */

            // write to out
            stdout.add(state.getID());
        }

        return null;
    }
 }
