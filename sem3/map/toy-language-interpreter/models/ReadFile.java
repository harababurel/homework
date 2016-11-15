package models;
import java.io.*;

public class ReadFile implements IStmt {
    private Exp var_file_id;
    private String var_name;

    public ReadFile(Exp var_file_id, String var_name) {
        this.var_file_id = var_file_id;
        this.var_name = var_name;
    }

    @Override
    public PrgState execute(PrgState state) {
        MyIDictionary <Integer, MyFile> fileTable = state.getFileTable();
        MyIDictionary <String, Integer> symTable = state.getSymTable();

        int var_file_id = this.var_file_id.eval();

        int fd = 
        BufferedReader reader = fileTable.
        

        return state;
    }
}
