package models;

public abstract class Exp {
    abstract public int eval(MyIDictionary <String, Integer> symTable,
                             MyIHeap heap) throws Exception;
    @Override
    abstract public String toString();
}
