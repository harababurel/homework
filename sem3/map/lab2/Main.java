import models.*;
import repo.*;
import ctrl.*;

public class Main {
    public static void main(String[] args) {
        /* Lab2Ex1:
         * v = 2;
         * print (v)
         */

        IStmt lab2ex1 = new CompStmt(
                new AssignStmt("v", new ConstExp(2)),
                new PrintStmt(new VarExp("v"))
                );
        IRepository r = new Repository(new PrgState(lab2ex1));
        Controller c = new Controller(r);
        c.allStep();
    }
}
