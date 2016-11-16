package models;
import java.io.*;

public class CloseRFileStmt implements IStmt {
    private Exp fd;

    public CloseRFileStmt(Exp fd) {
        this.fd = fd;
    }

    @Override
    public PrgState execute(PrgState state) {
        MyIDictionary <Integer, MyFile> fileTable = state.getFileTable();
        MyIDictionary <String, Integer> symTable = state.getSymTable();

        int fd = 0;
        try {
            fd = this.fd.eval(symTable);
        } catch(Exception e) {
            System.err.printf("Could not evaluate expression: %s\n", this.fd.toString());
            System.exit(1);
        }

        BufferedReader reader = fileTable.get(fd).getReader();
        if(reader == null) {
            System.err.printf("File descriptor %d does not exist.\n", fd);
            System.exit(1);
        }

        try {
            reader.close();
        } catch(Exception e) {
            System.err.printf("Could not close reader. Moving on.", fd);
        }

        fileTable.remove(fd);

        return state;
    }

    @Override
    public String toString() {
        return "CloseRFile(" + this.fd.toString() + ")";
    }
}
