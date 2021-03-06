package view;
import models.*;
import repo.*;
import ctrl.*;
import java.util.*;

public class TextMenu {
    private Map <String, Command> commands;

    public TextMenu() {
        this.commands = new HashMap <String, Command>();
    }

    public void addCommand(Command c) {
        this.commands.put(c.getKey(), c);
    }

    private void printMenu() {
        for(Command c:commands.values()) {
            String line = String.format("%4s: %s", c.getKey(), c.getDescription());
            System.out.println(line);
        }
    }

    public void run(String[] args) {
        Scanner scanner = new Scanner(System.in);

        while(true) {
            printMenu();

            String choice = scanner.nextLine();
            Command c = commands.get(choice);

            if(c == null) {
                System.out.println("Invalid command");
                continue;
            }

            c.execute();
        }
    }

    /* public void showOptions() { */
    /*     System.out.println("Choose sample program:"); */
    /*     System.out.println("\t(1): v=2; print(v)"); */
    /*     System.out.println("\t(2): a=2+3*5; b=a-4/2+7; print(b)"); */
    /*     System.out.println("\t(3): a=2-2; if a then v=2 else v=3; print(v)"); */
    /*     System.out.println("\t(4): lab5ex1"); */
    /*     System.out.println("\t(5): lab5ex2"); */

    /*     System.out.print("Program: "); */
    /* } */

    public static Controller generateController(String logFilePath, IStmt initialStmt) {
        return new Controller(new Repository(logFilePath, new PrgState(initialStmt)));
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

    public static IStmt generateExample4() {
        /* openRFile(var_f,"test.in");
         * readFile(var_f,var_c);print(var_c);
         * (if var_c then readFile(var_f,var_c);print(var_c)
         *  else print(0));
         * closeRFile(var_f)
         */
        IStmt lab5ex1 = new CompStmt(
                new OpenRFileStmt("var_f", "test.in"),
                new CompStmt(
                    new ReadFileStmt(new VarExp("var_f"), "var_c"),
                    new CompStmt(
                        new PrintStmt(new VarExp("var_c")),
                        new CompStmt(
                            new IfStmt(
                                new VarExp("var_c"),
                                new CompStmt(
                                    new ReadFileStmt(
                                        new VarExp("var_f"),
                                        "var_c"),
                                    new PrintStmt(new VarExp("var_c"))),
                                new PrintStmt(new ConstExp(0))),
                            new CloseRFileStmt(new VarExp("var_f"))))));
        return lab5ex1;
    }

    public static IStmt generateExample5() {
        /* openRFile(var_f,"test.in");
         * readFile(var_f+2,var_c);print(var_c);
         * (if var_c then readFile(var_f,var_c);print(var_c)
         * else print(0));
         * closeRFile(var_f)
         */

        IStmt lab5ex2 = new CompStmt(
                new OpenRFileStmt("var_f", "test.in"),
                new CompStmt(
                    new ReadFileStmt(
                        new ArithExp(
                            new VarExp("var_f"),
                            new ConstExp(2),
                            '+'),
                        "var_c"),
                    new CompStmt(
                        new PrintStmt(new VarExp("var_c")),
                        new CompStmt(
                            new IfStmt(
                                new VarExp("var_c"),
                                new CompStmt(
                                    new ReadFileStmt(
                                        new VarExp("var_f"),
                                        "var_c"),
                                    new PrintStmt(new VarExp("var_c"))),
                                new PrintStmt(new ConstExp(0))),
                            new PrintStmt(new VarExp("var_f"))))));
        return lab5ex2;
    }

    public static IStmt generateExample6() {
        /* v=10;
         * new(v,20);
         * new(a,22);
         * wH(a,30);
         * print(a);
         * print(rH(a));
         * a=0
         *
         * At the end of execution:
         * Heap={1->20}, SymTable={v->1, a->0} and Out={2,30}
         */

        IStmt lab6ex1 = new CompStmt(
                new AssignStmt("v", new ConstExp(10)),
                new CompStmt(
                    new NewStmt("v", new ConstExp(20)),
                    new CompStmt(
                        new NewStmt("a", new ConstExp(22)),
                        new CompStmt(
                            new WriteHeapStmt("a", new ConstExp(30)),
                            new CompStmt(
                                new PrintStmt(new VarExp("a")),
                                new CompStmt(
                                    new PrintStmt(new ReadHeapExp("a")),
                                    new AssignStmt("a", new ConstExp(0))))))));
        return lab6ex1;
    }

    public static IStmt generateExample7() {
        /* v = 6;
         * (while(v-4) print(v); v=v-1);
         * print(v);
         */

        IStmt lab7ex1 = new CompStmt(
                new AssignStmt("v", new ConstExp(6)),
                new CompStmt(
                    new WhileStmt(
                        new ArithExp(
                            new VarExp("v"),
                            new ConstExp(4),
                            '-'),
                        new CompStmt(
                            new PrintStmt(new VarExp("v")),
                            new AssignStmt(
                                "v",
                                new ArithExp(
                                    new VarExp("v"),
                                    new ConstExp(1),
                                    '-')))),
                    new PrintStmt(new VarExp("v"))));

        return lab7ex1;
    }

    public static IStmt generateExample8() {
        /* v=10;new(a,22);
         * fork(wH(a,30);v=32;print(v);print(rH(a)));
         * print(v);print(rH(a))
         */

        IStmt lab8ex1 = new CompStmt(
                new AssignStmt("v", new ConstExp(10)),
                new CompStmt(
                    new NewStmt("a", new ConstExp(22)),
                    new CompStmt(
                        new ForkStmt(
                            new CompStmt(
                                new WriteHeapStmt("a", new ConstExp(30)),
                                new CompStmt(
                                    new AssignStmt("v", new ConstExp(32)),
                                    new CompStmt(
                                        new PrintStmt(new VarExp("v")),
                                        new PrintStmt(new ReadHeapExp("a")))))),
                        new CompStmt(
                            new PrintStmt(new VarExp("v")),
                            new PrintStmt(new ReadHeapExp("a"))))));

        return lab8ex1;
    }

    public static IStmt generateExample9() {
        /* v=0; */
        /* (repeat (fork(print(v);v=v-1);v=v+1) until v==3); */
        /* x=1;y=2;z=3;w=4; */
        /* print(v*10) */

        IStmt practicalExamStmt = new CompStmt(
                new AssignStmt("v", new ConstExp(0)),
                new CompStmt(
                    new RepeatStmt(
                        new CompStmt(
                            new ForkStmt(
                                new CompStmt(
                                    new PrintStmt(new VarExp("v")),
                                    new AssignStmt("v", new ArithExp(new VarExp("v"), new ConstExp(1), '-'))
                                )
                            ),
                            new AssignStmt("v", new ArithExp(new VarExp("v"), new ConstExp(1), '+'))
                        ),
                        new CmpExp(new VarExp("v"), "==", new ConstExp(3))),
                    new CompStmt(
                        new AssignStmt("x", new ConstExp(1)),
                        new CompStmt(
                            new AssignStmt("y", new ConstExp(2)),
                            new CompStmt(
                                new AssignStmt("z", new ConstExp(3)),
                                new CompStmt(
                                    new AssignStmt("w", new ConstExp(4)),
                                    new PrintStmt(new ArithExp(new VarExp("v"), new ConstExp(10), '*'))))))));



        return practicalExamStmt;
    }

    public static IStmt generateExample10() {
        /* new(v1,2);new(v2,3);new(v3,4);newLatch(cnt,rH(v2)); */
        /* fork( */
        /*     wh(v1, rh(v1)*10); */
        /*     print(rh(v1)); */
        /*     countDown(cnt) */
        /* ); */
        /* fork(wh(v2,rh(v2)*10);print(rh(v2));countDown(cnt)); */
        /* fork(wh(v3,rh(v3)*10);print(rh(v3));countDown(cnt)); */
        /* await(cnt); */
        /* print(cnt); */
        /* countDown(cnt); */
        /* print(cnt) */

        IStmt practicalExamStmt = new CompStmt(
                new NewStmt("v1", new ConstExp(2)),
                new CompStmt(
                    new NewStmt("v2", new ConstExp(3)),
                    new CompStmt(
                        new NewStmt("v3", new ConstExp(4)),
                        new CompStmt(
                            new LatchStmt("cnt", new ReadHeapExp("v2")),
                            new CompStmt(
                                new ForkStmt(
                                    new CompStmt(
                                        new WriteHeapStmt("v1", new ArithExp(new ReadHeapExp("v1"), new ConstExp(10), '*')),
                                        new CompStmt(
                                            new PrintStmt(new ReadHeapExp("v1")),
                                            new CountdownStmt("cnt")))),
                                new CompStmt(
                                    new ForkStmt(
                                        new CompStmt(
                                            new WriteHeapStmt("v2", new ArithExp(new ReadHeapExp("v2"), new ConstExp(10), '*')),
                                            new CompStmt(
                                                new PrintStmt(new ReadHeapExp("v2")),
                                                new CountdownStmt("cnt")))),
                                    new CompStmt(
                                        new ForkStmt(
                                            new CompStmt(
                                                new WriteHeapStmt("v3", new ArithExp(new ReadHeapExp("v3"), new ConstExp(10), '*')),
                                                new CompStmt(
                                                    new PrintStmt(new ReadHeapExp("v3")),
                                                    new CountdownStmt("cnt")))),
                                        new CompStmt(
                                            new AwaitStmt("cnt"),
                                            new CompStmt(
                                                new PrintStmt(new ReadHeapExp("cnt")),
                                                /* new PrintStmt(new VarExp("cnt")), */

                                                new CompStmt(
                                                    new CountdownStmt("cnt"),
                                                    new PrintStmt(new ReadHeapExp("cnt"))
                                                    /* new PrintStmt(new VarExp("cnt")) */
                                                    ))))))))));

        return practicalExamStmt;
    }


        /* v=10;new(a,22);
    /* public void run() { */
    /*     Scanner scanner = new Scanner(System.in); */

    /*     while(true) { */
    /*         showOptions(); */

    /*         String choice = scanner.nextLine(); */

    /*         switch(choice) { */
    /*             case "1": */
    /*                 runExample("main.log", generateExample1()); */
    /*                 break; */
    /*             case "2": */
    /*                 runExample("main.log", generateExample2()); */
    /*                 break; */
    /*             case "3": */
    /*                 runExample("main.log", generateExample3()); */
    /*                 break; */
    /*             case "4": */
    /*                 runExample("main.log", generateExample4()); */
    /*                 break; */
    /*             case "5": */
    /*                 runExample("main.log", generateExample5()); */
    /*                 break; */
    /*         } */
    /*     } */
    /* } */
}
