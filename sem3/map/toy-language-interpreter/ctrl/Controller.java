package ctrl;
import models.*;
import repo.*;
import java.io.*;
import java.util.*;
import java.util.stream.*;

public class Controller {
    private IRepository r;

    public Controller() {
        this.r = new Repository("");
    }

    public Controller(IRepository r) {
        this.r = r;
    }

    MyDictionary <Integer,Integer> conservativeGarbageCollector(Collection <Integer> symTableValues,
                                                      MyIHeap heap) {
        MyDictionary <Integer, Integer> new_content = new MyDictionary <Integer, Integer>();
        for(Map.Entry <Integer, Integer> x:heap.entrySet().stream()
                                                          .filter(e -> symTableValues.contains(e.getKey()))
                                                          .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue))
                                                          .entrySet())
            new_content.put(x.getKey(), x.getValue());
        return new_content;
    }

    public PrgState oneStep(PrgState state) throws Exception {
        MyIStack <IStmt> exeStack = state.getExeStack();

        // if(exeStack.isEmpty())
            // return null;
            // throws MyStmtExecException;
        IStmt currentStmt = exeStack.pop();
        return currentStmt.execute(state);
    }

    public void allStep() throws Exception {
        PrgState state = r.getCurrentState();

        // initial state
        System.out.println(state.toString());
        try {
            this.r.logPrgStateExec();
        } catch(IOException e) { ; }

        while(!state.getExeStack().isEmpty()) {
            oneStep(state);
            state.getHeap().setContent(conservativeGarbageCollector(
                        state.getSymTable().values(),
                        state.getHeap()));
            System.out.println(state.toString());
            try {
                this.r.logPrgStateExec();

                this.r.serialize(File.createTempFile("toy_language_interpreter_", ".ser").getAbsolutePath());
            } catch(IOException e) { ; }
        }
    }
}
