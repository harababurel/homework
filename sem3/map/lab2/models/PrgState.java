package models;

public class PrgState {
    private MyIStack <IStmt> exeStack;
    private MyIDictionary <String, Integer> symTable;
    private MyIList <Integer> stdout;
    private IStmt initialProgram; //optional field, but good to have

    public PrgState(MyIStack <IStmt> exeStack,
            MyIDictionary <String, Integer> symTable,
            MyIList <Integer> stdout,
            IStmt initialProgram) {
        this.exeStack = exeStack;
        this.symTable = symTable;
        this.stdout = stdout;
        this.initialProgram = initialProgram;
        this.exeStack.push(initialProgram);
    }

    public PrgState(IStmt initialProgram) {
        this.exeStack = new MyStack <IStmt>();
        this.symTable = new MyDictionary <String, Integer>();
        this.stdout = new MyList <Integer>();
        this.initialProgram = initialProgram;
        this.exeStack.push(initialProgram);
    }

    @Override
    public String toString() {
        return "/============== PrgState =================\n" +
            "exeStack:\n" + this.exeStack.toString() + "\n" +
            "symTable:\n" + this.symTable.toString() + "\n" +
            "stdout:\n" + this.stdout.toString() +
            "\\=========================================\n";
    }

    public MyIStack <IStmt> getExeStack() {
        return this.exeStack;
    }

    public MyIDictionary <String, Integer> getSymTable() {
        return this.symTable;
    }

    public MyIList <Integer> getStdout() {
        return this.stdout;
    }

    public IStmt getinitialProgram() {
        return this.initialProgram;
    }

    public void setExeStack(MyIStack <IStmt> exeStack) {
        this.exeStack = exeStack;
    }

    public void setSymTable(MyIDictionary <String, Integer> symTable) {
        this.symTable = symTable;
    }

    public void setStdout(MyIList <Integer> stdout) {
        this.stdout = stdout;
    }

    public void setinitialProgram(IStmt initialProgram) {
        this.initialProgram = initialProgram;
    }
}
