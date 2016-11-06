package model;

public class IfStmt implements IStmt {
    private Exp exp;
    private IStmt thenStmt, elseStmt;

    public IfStmt(Exp exp, IStmt thenStmt, IStmt elseStmt) {
        this.exp = exp;
        this.thenStmt = thenStmt;
        this.elseStmt = elseStmt;
    }

    @Override
    public String toString() {
        return "IF(" + exp.toString() + ") THEN(" + thenStmt.toStr()
            + ") ELSE(" + elseStmt.toString() + ")";
    }

    @Override
    public PrgState execute(PrgState state) {
        return state;
    }
}
