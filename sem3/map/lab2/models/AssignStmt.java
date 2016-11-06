package model;

public class AssignStmt implements IStmt {
    private String id;
    private Exp exp;

    public AssignStmt(String id, Exp exp) {
        this.id = id;
        this.exp = exp;
    }

    @Override
    public String toString() {
        return id + " = " + exp.toString();
    }

    @Override
    public PrgState execute(PrgState state) {
        MyIStack <IStmt> stack = state.getStack();
        MyIDictionary <String, Integer> symTable = state.getSymTable();

        int val = exp.eval(symTable);
        if(symTable.isDefined(id))
            symTable.update(id, val);
        else
            symTable.add(id, val);

        return state;
    }
}
