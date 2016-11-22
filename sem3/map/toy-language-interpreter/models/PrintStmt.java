package models;

public class PrintStmt implements IStmt {
    private Exp exp;

    public PrintStmt(Exp exp) {
        this.exp = exp;
    }

    @Override
    public String toString() {
        return "print(" + this.exp.toString() + ")";
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        MyIStack <IStmt> stack = state.getExeStack();
        MyIList <Integer> stdout = state.getStdout();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIHeap heap = state.getHeap();

        stdout.add(this.exp.eval(symTable, heap));

        return state;
    }
 }
