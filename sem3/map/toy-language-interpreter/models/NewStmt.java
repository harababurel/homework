package models;

public class NewStmt implements IStmt {
    private String var_name;
    private Exp exp;

    public NewStmt(String var_name, Exp exp) {
        this.var_name = var_name;
        this.exp = exp;
    }

    @Override
    public String toString() {
        return "new(" + this.var_name + ", " + this.exp.toString() + ")";
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        MyIHeap heap = state.getHeap();
        MyIDictionary <String, Integer> symTable = state.getSymTable();

        int new_location = heap.alloc();
        System.out.printf("Allocated memory at location %d\n", new_location);
        int new_value = this.exp.eval(symTable, heap);

        heap.write_location(new_location, new_value);
        symTable.put(this.var_name, new_location);

        return state;
    }
}
