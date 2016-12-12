package repo;
import models.*;
import java.io.*;

public interface IRepository {
    /* PrgState getCurrentState(); */
    MyList <PrgState> getPrgList();
    void setPrgList(MyList <PrgState> newPrgList);
    void serialize(String filename);
    /* static PrgState deserialize(String filename); */
    void logPrgStateExec(PrgState state) throws IOException;
}
