package models;
import java.util.*;
import java.io.*;

public class RepeatStmt implements IStmt, Serializable {
    private Exp exp;
    private IStmt stmt;

    public RepeatStmt(IStmt stmt, Exp exp) {
        this.stmt = stmt;
        this.exp = exp;
    }

    @Override
    public String toString() {
        return "repeat " + this.stmt.toString() + " until " + this.exp.toString();
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        MyIStack <IStmt> exeStack = state.getExeStack();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIHeap heap = state.getHeap();


        exeStack.push(
                new CompStmt(
                    this.stmt,
                    new WhileStmt(
                        new CmpExp(this.exp, "==", new ConstExp(0)),
                        this.stmt)
                    )
                );

        /*
        if(this.exp.eval(symTable, heap) != 0) {
            exeStack.push(this);
            exeStack.push(this.stmt);
        }*/

        return null;
    }
}
