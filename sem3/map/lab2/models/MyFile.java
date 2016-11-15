package models;
import java.io.*;

public class MyFile {
    public String filename;
    public BufferedReader reader;

    public MyFile(String filename, BufferedReader reader) {
        this.filename = filename;
        this.reader = reader;
    }
}
    


