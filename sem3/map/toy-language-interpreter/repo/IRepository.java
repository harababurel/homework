package repo;
import models.*;
import java.io.*;

public interface IRepository {
    PrgState getCurrentState();
    void serialize(String filename);
    /* static PrgState deserialize(String filename); */
    void logPrgStateExec() throws IOException;
}
