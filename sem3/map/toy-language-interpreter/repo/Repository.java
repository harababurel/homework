package repo;
import models.*;
import java.util.*;
import java.io.*;
import java.nio.file.*;

public class Repository implements IRepository, Serializable {
    private List <PrgState> states;
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
        this.states = new LinkedList <PrgState>();
    }

    public Repository(String logFilePath, PrgState initialState) {
        prepareLogFilePath(logFilePath);
        this.states = new LinkedList <PrgState>();
        this.states.add(initialState);
    }

    public void serialize(String filename) {
        try {
            FileOutputStream fileOut = new FileOutputStream(filename);
            ObjectOutputStream out = new ObjectOutputStream(fileOut);

            out.writeObject(this);

            out.close();
            fileOut.close();

            System.out.printf("Serialized data saved as %s\n", filename);
        } catch(IOException e) {
            e.printStackTrace();
        }
    }

    public static Repository deserialize(String filename) {
        Repository deserializedRepo = null;
        try {
            FileInputStream fileIn = new FileInputStream(filename);
            ObjectInputStream in = new ObjectInputStream(fileIn);

            deserializedRepo = (Repository)in.readObject();

            in.close();
            fileIn.close();

            System.out.printf("Deserialized repository from %s.\n", filename);
        } catch(IOException i) {
            i.printStackTrace();
        } catch(ClassNotFoundException i) {
            i.printStackTrace();
        }

        return deserializedRepo;
    }

    /* Starting with lab 8, this is no longer used. */
    /* public PrgState getCurrentState() { */
        // LinkedList implements getLast(), but this is
        // a more general List.
        /* return this.states.get(this.states.size()-1); */
    /* } */

    public List <PrgState> getPrgList() {
        return this.states;
    }

    public void setPrgList(List <PrgState> newPrgList) {
        this.states = newPrgList;
    }

    public void logPrgStateExec(PrgState state) throws IOException {
        /* System.out.printf("logPrgState: %d\n", state.getID()); */
        try {
            PrintWriter logFile = new PrintWriter(new BufferedWriter(new FileWriter(this.logFilePath, true)));

            logFile.println(state.toString());
            logFile.close();
        } catch(Exception e) {
            System.err.println(e);
            throw e;
        }
    }
}
