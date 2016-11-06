package model;

public abstract class Exp {
    abstract int eval(MyIDictionary <String, Integer> symTable);
    abstract String toString();
}
