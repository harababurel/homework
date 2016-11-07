package models;

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
        return "IF(" + this.exp.toString() + ") THEN(" + this.thenStmt.toString()
            + ") ELSE(" + this.elseStmt.toString() + ")";
    }

    @Override
    public PrgState execute(PrgState state) {
        return state;
    }
}
