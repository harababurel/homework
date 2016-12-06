package models;
import java.util.*;
import java.io.*;

public class WhileStmt implements IStmt, Serializable {
    private Exp exp;
    private IStmt stmt;

    public WhileStmt(Exp exp, IStmt stmt) {
        this.exp = exp;
        this.stmt = stmt;
    }

    @Override
    public String toString() {
        return "while(" + this.exp.toString() + ") " + this.stmt.toString();
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        MyIStack <IStmt> exeStack = state.getExeStack();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIHeap heap = state.getHeap();

        if(this.exp.eval(symTable, heap) != 0) {
            exeStack.push(this);
            exeStack.push(this.stmt);
        }

        return state;
    }
}
