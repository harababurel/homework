import models.*;
import repo.*;
import ctrl.*;

public class Main {
    public static IStmt generateExample1() {
        /* Lab2Ex1:
         * v = 2;
         * print (v)
         */

        IStmt lab2ex1 = new CompStmt(
                new AssignStmt("v", new ConstExp(2)),
                new PrintStmt(new VarExp("v"))
                );
        return lab2ex1;
    }

    public static IStmt generateExample2() {
        /* a=2+3*5;
         * b=a-4/2+7;
         * Print(b)
         */

        IStmt lab2ex2 = new CompStmt(
                new AssignStmt(
                    "a",
                    new ArithExp(
                        new ConstExp(2),
                        new ArithExp(
                            new ConstExp(3),
                            new ConstExp(5),
                            '*'),
                        '+')
                    ),
                new CompStmt(
                    new AssignStmt(
                        "b",
                        new ArithExp(
                            new ArithExp(
                                new VarExp("a"),
                                new ArithExp(
                                    new ConstExp(-4),
                                    new ConstExp(2),
                                    '/'),
                                '+'),
                            new ConstExp(7),
                            '+')),
                    new PrintStmt(new VarExp("b"))
                    )
                );
        return lab2ex2;
    }

    public static void runExample(IStmt initialStmt) {
        IRepository r = new Repository(new PrgState(initialStmt));
        Controller c = new Controller(r);
        c.allStep();
    }

    public static void main(String[] args) {
        runExample(generateExample2());
    }
}
