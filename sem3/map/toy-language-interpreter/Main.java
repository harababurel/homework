import models.*;
import repo.*;
import ctrl.*;
import view.*;
import view.TextMenu.*;
import java.util.*;

public class Main {
    public static void main(String[] args) {
        TextMenu menu = new TextMenu();

        String logFilePath = "main.log";

        menu.addCommand(new RunExample("1", "Lab2ex1", TextMenu.generateController(logFilePath, TextMenu.generateExample1())));
        menu.addCommand(new RunExample("2", "Lab2ex2", TextMenu.generateController(logFilePath, TextMenu.generateExample2())));
        menu.addCommand(new RunExample("3", "Lab2ex2", TextMenu.generateController(logFilePath, TextMenu.generateExample3())));
        menu.addCommand(new RunExample("4", "Lab5ex1", TextMenu.generateController(logFilePath, TextMenu.generateExample4())));
        menu.addCommand(new RunExample("5", "Lab5ex2", TextMenu.generateController(logFilePath, TextMenu.generateExample5())));
        menu.addCommand(new RunExample("6", "Lab6ex1", TextMenu.generateController(logFilePath, TextMenu.generateExample6())));

        menu.run();
    }
}
