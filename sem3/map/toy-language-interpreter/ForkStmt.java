package models;
import java.io.*;

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

        PrgState forkedState = new PrgState(this.ForkStmt);
        forkedState.setSymTable(state.getSymTable().clone());
        forkedState.setHeap(state.getHeap());
        forkedState.setFileTable(state.getFileTable());
        forkedState.setStdout(state.getStdout());
        forkedState.setID(state.getID() * 10);

        return forkedState;
    }
}
