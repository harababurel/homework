package models;

public interface IStmt {
    public PrgState execute(PrgState state) throws Exception;
}
