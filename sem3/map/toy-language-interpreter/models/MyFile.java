package models;
import java.io.*;

public class MyFile implements Serializable {
    private String filename;
    transient private BufferedReader reader;

    public MyFile(String filename, BufferedReader reader) {
        this.filename = filename;
        this.reader = reader;
    }

    public String getFilename() {
        return this.filename;
    }

    public BufferedReader getReader() {
        return this.reader;
    }

    public void setFilename(String filename) {
        this.filename = filename;
    }

    public void setReader(BufferedReader reader) {
        this.reader = reader;
    }

    @Override
    public String toString() {
        return "MyFile(\"" + this.filename + "\", " + this.reader.toString() + ")";
    }
}
