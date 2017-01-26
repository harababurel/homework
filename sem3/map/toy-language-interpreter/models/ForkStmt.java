package models;
import java.io.*;
import java.math.*;

public class ForkStmt implements IStmt, Serializable {
    private IStmt forkedStmt;

    public ForkStmt(IStmt forkedStmt) {
        this.forkedStmt = forkedStmt;
    }

    @Override
    public String toString() {
        return "fork(" + forkedStmt.toString() + ")";
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        PrgState forkedState = new PrgState(
                new MyStack <IStmt>(),
                state.getSymTable().clone(),
                state.getLatchTable(),
                state.getStdout(),
                state.getFileTable(),
                state.getHeap(),
                this.forkedStmt);

        forkedState.setID(state.getID()*10 + (int)(Math.random() * 100 + 1));

        /* System.out.println(forkedState.getExeStack()); */

        return forkedState;
    }
}
