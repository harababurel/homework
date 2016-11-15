package repo;
import models.*;
import java.util.*;
import java.io.*;

public class Repository implements IRepository {
    private MyList <PrgState> states;
    private String logFilePath;

    public void readLogFilePath() {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Log file: ");
        this.logFilePath = scanner.nextLine();
    }

    public Repository() {
        readLogFilePath();
        this.states = new MyList <PrgState>();
    }

    public Repository(PrgState initialState) {
        readLogFilePath();
        this.states = new MyList <PrgState>();
        this.states.add(initialState);
    }

    public PrgState getCurrentState() {
        // LinkedList implements getLast(), but this is
        // a more general List.
        return this.states.get(this.states.size()-1);
    }

    public void logPrgStateExec() throws IOException {
        try {
            PrintWriter logFile = new PrintWriter(new BufferedWriter(new FileWriter(this.logFilePath, true)));

            logFile.println(this.getCurrentState().toString());
            logFile.close();
        } catch(Exception e) {
                System.err.println(e);
                throw e;
        }
    }
}
