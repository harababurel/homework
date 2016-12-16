package models;
import java.io.*;

public class OpenRFileStmt implements IStmt, Serializable {
    private String variable; // name of variable
    private String filename;

    public OpenRFileStmt(String variable, String filename) {
        this.variable = variable;
        this.filename = filename;
    }

    @Override
    public PrgState execute(PrgState state) {
        MyIDictionary <Integer, MyFile> fileTable = state.getFileTable();
        MyIDictionary <String, Integer> symTable = state.getSymTable();

        for(MyFile entry:fileTable.values()) {
            if(entry.getFilename() == this.filename) {
                System.err.println("File is already open");
                System.exit(1);
            }
        }

        try {
            BufferedReader reader = new BufferedReader(new FileReader(this.filename));
            // Assign the first unused file descriptor to this file.
            for(int fd=3; ; fd++)
                if(!fileTable.containsKey(fd)) {
                    fileTable.put(fd, new MyFile(this.filename, reader));
                    symTable.put(this.variable, fd);
                    break;
                }
        } catch(IOException e) {
            System.err.printf("Could not open %s for reading.\n", this.filename);
            System.err.printf(e.toString());
            System.exit(1);
        }

        return null;
    }

    @Override
    public String toString() {
        return "OpenRFile(" + this.variable + ", \"" + this.filename + "\")";
    }

}
