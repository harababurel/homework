package models;

public class CompStmt implements IStmt {
    private IStmt fst, snd;

    public CompStmt(IStmt fst, IStmt snd) {
        this.fst = fst;
        this.snd = snd;
    }

    @Override
    public String toString() {
        return "(" + this.fst.toString() + "; " + this.snd.toString() + ")";
    }

    @Override
    public PrgState execute(PrgState state) {
        MyIStack <IStmt> stack = state.getExeStack();

        stack.push(this.snd);
        stack.push(this.fst);

        return state;
    }
}
