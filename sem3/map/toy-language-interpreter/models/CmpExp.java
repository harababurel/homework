package models;
import java.io.*;

public class CmpExp extends Exp implements Serializable {
    private Exp lterm, rterm;
    private String op;

    public CmpExp(Exp lterm, String op, Exp rterm) {
        this.lterm = lterm;
        this.rterm = rterm;
        this.op = op;
    }

    @Override
    public int eval(MyIDictionary <String, Integer> symTable,
                    MyIHeap heap) {

        boolean ret = false;

        try {
            int lval = this.lterm.eval(symTable, heap);
            int rval = this.rterm.eval(symTable, heap);

            switch(this.op) {
                case "<":  ret = lval <  rval;
                case "<=": ret = lval <= rval;
                case ">":  ret = lval >  rval;
                case ">=": ret = lval >= rval;
                case "==": ret = lval == rval;
                case "!=": ret = lval != rval;
            }
        } catch(Exception e) {
            System.out.println(e);
        }

        return ret? 1:0;
    }

    @Override
    public String toString() {
        return this.lterm.toString() + " " + this.op + " " + this.lterm.toString();
    }
}
