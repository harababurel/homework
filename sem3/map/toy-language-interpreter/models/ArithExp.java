package models;
import java.io.*;

public class ArithExp extends Exp implements Serializable {
    private Exp e1, e2;
    private char op;

    public ArithExp(Exp e1, Exp e2, char op) {
        this.e1 = e1;
        this.e2 = e2;
        this.op = op;
    }

    @Override
    public int eval(MyIDictionary <String, Integer> symTable,
                    MyIHeap heap) throws Exception {
        int partialRes1 = this.e1.eval(symTable, heap);
        int partialRes2 = this.e2.eval(symTable, heap);

        switch(this.op) {
            case '+': return partialRes1 + partialRes2;
            case '-': return partialRes1 - partialRes2;
            case '*': return partialRes1 * partialRes2;
            case '/': return partialRes1 / partialRes2;
        }

        return 0;
    }

    @Override
    public String toString() {
        return this.e1.toString() + " " + this.op + " " + this.e2.toString();
    }
}
