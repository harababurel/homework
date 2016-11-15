package models;
import java.io.*;

public class OpenRFileStmt implements IStmt {
    private String var_file_id; // name of variable
    private String filename;

    public OpenRFileStmt(String var_file_id, String filename) {
        this.var_file_id = var_file_id;
        this.filename = filename;
    }

    @Override
    public PrgState execute(PrgState state) {
        MyIDictionary <Integer, MyFile> fileTable = state.getFileTable();

        for(MyFile entry:fileTable.values()) {
            if(entry.filename == this.filename) {
                System.err.println("File is already open");
                System.exit(1);
            }
        }

        try {
            BufferedReader reader = new BufferedReader(new FileReader(this.filename));
        } catch(Exception e) {
            System.err.printf("Could not open %s for reading.\n", this.filename);
        }
        return state;
    }
}


    
