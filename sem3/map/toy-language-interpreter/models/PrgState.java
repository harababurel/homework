package models;

public class PrgState {
    private MyIStack <IStmt> exeStack;
    private MyIDictionary <String, Integer> symTable;
    private MyIList <Integer> stdout;
    private MyIDictionary <Integer, MyFile> fileTable;
    private MyIHeap heap;
    private IStmt initialProgram; //optional field, but good to have

    public PrgState(MyIStack <IStmt> exeStack,
            MyIDictionary <String, Integer> symTable,
            MyIList <Integer> stdout,
            MyIDictionary <Integer, MyFile> fileTable,
            MyIHeap heap,
            IStmt initialProgram) {
        this.exeStack = exeStack;
        this.symTable = symTable;
        this.stdout = stdout;
        this.fileTable = fileTable;
        this.heap = heap;
        this.initialProgram = initialProgram;
        this.exeStack.push(initialProgram);
    }

    public PrgState(IStmt initialProgram) {
        this.exeStack = new MyStack <IStmt>();
        this.symTable = new MyDictionary <String, Integer>();
        this.stdout = new MyList <Integer>();
        this.fileTable = new MyDictionary <Integer, MyFile>();
        this.heap = new MyHeap();
        this.initialProgram = initialProgram;
        this.exeStack.push(initialProgram);
    }

    @Override
    public String toString() {
        return "exeStack:\n" + this.exeStack.toString() +
               "symTable:\n" + this.symTable.toString() +
               "fileTable:\n" + this.fileTable.toString() +
               "heap:\n" + this.heap.toString() +
               "stdout:\n" + this.stdout.toString();
            
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

    public MyIDictionary <Integer, MyFile> getFileTable() {
        return this.fileTable;
    }

    public MyIHeap getHeap() {
        return this.heap;
    }

    public IStmt getInitialProgram() {
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

    public void setFileTable(MyIDictionary <Integer, MyFile> fileTable) {
        this.fileTable = fileTable;
    }

    public void setHeap(MyIHeap heap) {
        this.heap = heap;
    }

    public void setInitialProgram(IStmt initialProgram) {
        this.initialProgram = initialProgram;
    }
}
