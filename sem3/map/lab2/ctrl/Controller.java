package ctrl;
import models.*;
import repo.*;

public class Controller {
    private IRepository r;

    public Controller() {
        this.r = new Repository();
    }

    public Controller(IRepository r) {
        this.r = r;
    }

    public PrgState oneStep(PrgState state) {
        MyIStack <IStmt> exeStack = state.getExeStack();

        // if(exeStack.isEmpty())
            // return null;
            // throws MyStmtExecException;
        IStmt currentStmt = exeStack.pop();
        return currentStmt.execute(state);
    }

    public void allStep() {
        PrgState state = r.getCurrentState();

        // initial state
        System.out.println(state.toString());

        while(!state.getExeStack().isEmpty()) {
            oneStep(state);
            System.out.println(state.toString());
        }
    }
}
