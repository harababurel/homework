package models;

public class PrgState {
    private MyIStack <IStmt> exeStack;
    private MyIDictionary <String, Integer> symTable;
    private MyIList <Integer> stdout;
    private IStmt originalProgram; //optional field, but good to have

    public PrgState(MyIStack <IStmt> exeStack,
            MyIDictionary <String, Integer> symTable,
            MyIList <Integer> stdout,
            IStmt originalProgram) {
        this.exeStack = exeStack;
        this.symTable = symTable;
        this.stdout = stdout;
        this.originalProgram = originalProgram;
        this.exeStack.push(originalProgram);
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

    public IStmt getOriginalProgram() {
        return this.originalProgram;
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

    public void setOriginalProgram(IStmt originalProgram) {
        this.originalProgram = originalProgram;
    }
}
