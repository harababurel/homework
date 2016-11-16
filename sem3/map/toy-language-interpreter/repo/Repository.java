package repo;
import models.*;
import java.util.*;
import java.io.*;
import java.nio.file.*;

public class Repository implements IRepository {
    private MyList <PrgState> states;
    private String logFilePath;

    public void prepareLogFilePath(String logFilePath) {
        if(logFilePath == "") {
            Scanner scanner = new Scanner(System.in);
            System.out.print("Log file: ");
            logFilePath = scanner.nextLine();
        }
        this.logFilePath = logFilePath;

        try {
            Files.deleteIfExists(Paths.get(this.logFilePath));
        } catch(Exception e) {
            System.err.printf("Could not delete file %s for future logging.\n", this.logFilePath);
        }
    }

    public Repository(String logFilePath) {
        prepareLogFilePath(logFilePath);
        this.states = new MyList <PrgState>();
    }

    public Repository(String logFilePath, PrgState initialState) {
        prepareLogFilePath(logFilePath);
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
            // TODO: first file open overwrites file.
            PrintWriter logFile = new PrintWriter(new BufferedWriter(new FileWriter(this.logFilePath, true)));

            logFile.println(this.getCurrentState().toString());
            logFile.close();
        } catch(Exception e) {
                System.err.println(e);
                throw e;
        }
    }
}
