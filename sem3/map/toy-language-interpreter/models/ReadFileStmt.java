package models;
import java.io.*;

public class ReadFileStmt implements IStmt {
    private Exp fd;
    private String target_var;

    public ReadFileStmt(Exp fd, String target_var) {
        this.fd = fd;
        this.target_var = target_var;
    }

    @Override
    public PrgState execute(PrgState state) throws Exception {
        MyIDictionary <Integer, MyFile> fileTable = state.getFileTable();
        MyIDictionary <String, Integer> symTable = state.getSymTable();
        MyIHeap heap = state.getHeap();

        int fd = this.fd.eval(symTable, heap);

        if(!fileTable.containsKey(fd)) {
            System.err.printf("File descriptor %d does not exist.\n", fd);
            System.exit(1);
        }

        BufferedReader reader = fileTable.get(fd).getReader();
        int val;

        try {
            val = Integer.parseInt(reader.readLine());
        } catch(Exception e) { 
            val = 0;
        }

        symTable.put(this.target_var, val);
        return state;
    }

    @Override
    public String toString() {
        return "ReadFile(" + this.fd.toString() + ", " + this.target_var + ")";
    }

}
