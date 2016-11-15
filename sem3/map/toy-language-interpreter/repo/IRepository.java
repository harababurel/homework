package repo;
import models.*;
import java.io.*;

public interface IRepository {
    PrgState getCurrentState();
    void logPrgStateExec() throws IOException;
}
