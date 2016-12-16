package repo;
import models.*;
import java.io.*;
import java.util.*;

public interface IRepository {
    /* PrgState getCurrentState(); */
    List <PrgState> getPrgList();
    void setPrgList(List <PrgState> newPrgList);
    void serialize(String filename);
    /* static PrgState deserialize(String filename); */
    void logPrgStateExec(PrgState state) throws IOException;
}
