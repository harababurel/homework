package models;
import java.io.*;
import java.math.*;
import java.util.*;

public class PrgState implements Serializable, Cloneable {
    private MyIStack <IStmt> exeStack;
    private MyIDictionary <String, Integer> symTable;
    private MyIDictionary <Integer, Integer> latchTable;
    private List <Integer> stdout;
    private MyIDictionary <Integer, MyFile> fileTable;
    private MyIHeap heap;
    private IStmt initialProgram; //optional field, but good to have
    private int id;

    public PrgState(MyIStack <IStmt> exeStack,
            MyIDictionary <String, Integer> symTable,
            MyIDictionary <Integer, Integer> latchTable,
            List <Integer> stdout,
            MyIDictionary <Integer, MyFile> fileTable,
            MyIHeap heap,
            IStmt initialProgram) {
        this.exeStack = exeStack;
        this.symTable = symTable;
        this.latchTable = latchTable;
        this.stdout = stdout;
        this.fileTable = fileTable;
        this.heap = heap;
        this.initialProgram = initialProgram;
        this.exeStack.push(initialProgram);
        this.id = (int)(Math.random() * 100 + 1);
    }

    public PrgState(IStmt initialProgram) {
        this.exeStack = new MyStack <IStmt>();
        this.symTable = new MyDictionary <String, Integer>();
        this.latchTable = new MyDictionary <Integer, Integer>();
        this.stdout = new LinkedList <Integer>();
        this.fileTable = new MyDictionary <Integer, MyFile>();
        this.heap = new MyHeap();
        this.initialProgram = initialProgram;
        this.exeStack.push(initialProgram);
        this.id = (int)(Math.random() * 100 + 1);
    }

    @Override
    public String toString() {
        return "PrgState id = " + Integer.toString(this.id) + "\n" +
               "exeStack:\n" + this.exeStack.toString() +
               "symTable:\n" + this.symTable.toString() +
               "latchTable:\n" + this.latchTable.toString() +
               "fileTable:\n" + this.fileTable.toString() +
               "heap:\n" + this.heap.toString() +
               "stdout:\n" + this.stdout.toString();
    }

    public int getID() {
        return this.id;
    }

    public MyIStack <IStmt> getExeStack() {
        return this.exeStack;
    }

    public MyIDictionary <String, Integer> getSymTable() {
        return this.symTable;
    }

    public MyIDictionary <Integer, Integer> getLatchTable() {
        return this.latchTable;
    }

    public List <Integer> getStdout() {
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

    public void setID(int id) {
        this.id = id;
    }

    public void setExeStack(MyIStack <IStmt> exeStack) {
        this.exeStack = exeStack;
    }

    public void setSymTable(MyIDictionary <String, Integer> symTable) {
        this.symTable = symTable;
    }

    public void setLatchTable(MyIDictionary <Integer, Integer> latchTable) {
        this.latchTable = latchTable;
    }


    public void setStdout(List <Integer> stdout) {
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

    public boolean isNotCompleted() {
        return !this.exeStack.isEmpty();
    }

    public PrgState oneStep() throws Exception {
        if(!this.isNotCompleted())
            throw new Exception("Execution stack is empty. Can't oneStep().");

        // execute the top statement
        return this.exeStack.pop().execute(this);
    }
}
