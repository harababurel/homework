package models;

public interface IStmt {
    public PrgState execute(PrgState state);
    @Override
    public String toString();
}
