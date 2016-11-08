package view;
import models.*;
import repo.*;
import ctrl.*;
import java.util.*;

public class Menu {
    public void showOptions() {
        System.out.println("Choose sample program:");
        System.out.println("\t(1): v=2; print(v)");
        System.out.println("\t(2): a=2+3*5; b=a-4/2+7; print(b)");
        System.out.println("\t(3): a=2-2; if a then v=2 else v=3; print(v)");

        System.out.print("Program: ");
    }

    public static void runExample(IStmt initialStmt) {
        IRepository r = new Repository(new PrgState(initialStmt));
        Controller c = new Controller(r);
        c.allStep();
    }

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

    public static IStmt generateExample3() {
        /* a=2-2;
         * If a Then v=2 Else v=3;
         * Print(v)
         */
        IStmt lab2ex3 = new CompStmt(
                new AssignStmt(
                    "a",
                    new ArithExp(
                        new ConstExp(2),
                        new ConstExp(2),
                        '-')
                    ),
                new CompStmt(
                    new IfStmt(
                        new VarExp("a"),
                        new AssignStmt(
                            "v",
                            new ConstExp(2)),
                        new AssignStmt(
                            "v",
                            new ConstExp(3))),
                    new PrintStmt(new VarExp("v"))
                    )
                );
        return lab2ex3;
    }


    public void run() {
        Scanner scanner = new Scanner(System.in);

        while(true) {
            showOptions();

            String choice = scanner.nextLine();
            switch(choice) {
                case "1":
                    runExample(generateExample1());
                    break;
                case "2":
                    runExample(generateExample2());
                    break;
                case "3":
                    runExample(generateExample3());
                    break;
            }
        }
    }
}
