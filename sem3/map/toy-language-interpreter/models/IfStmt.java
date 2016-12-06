package models;
import java.io.*;

public class IfStmt implements IStmt, Serializable {
    private Exp exp;
    private IStmt thenStmt, elseStmt;

    public IfStmt(Exp exp, IStmt thenStmt, IStmt elseStmt) {
        this.exp = exp;
        this.thenStmt = thenStmt;
        this.elseStmt = elseStmt;
    }

    @Override
    public String toString() {
        return "if(" + this.exp.toString() + ") then(" + this.thenStmt.toString() + ") else(" + this.elseStmt.toString() + ")";
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        MyIStack <IStmt> exeStack = state.getExeStack();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIHeap heap = state.getHeap();

        if(this.exp.eval(symTable, heap) != 0)
            exeStack.push(this.thenStmt);
        else
            exeStack.push(this.elseStmt);

        return state;
    }
}
