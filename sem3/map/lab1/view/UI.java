package view;

import controller.Controller;
import model.Cube;
import model.Cylinder;
import model.Sphere;
import model.GeometricShape;

import java.util.Scanner;

public class UI {
    public Controller c;
    Scanner scanner = new Scanner(System.in);

    public UI(Controller c) {
        this.c = c;
    }

    void menu() {
        System.out.println("Operations:");
        System.out.println("\tadd");
        System.out.println("\tremove");
        System.out.println("\tsolve");
        System.out.println("\texit");
    }

    String readString() {
        return this.scanner.nextLine().trim();
    }

    int readInt() {
        int ret = this.scanner.nextInt();
        this.scanner.nextLine();
        return ret;
    }

    double readDouble() {
        double ret = this.scanner.nextDouble();
        this.scanner.nextLine();
        return ret;
    }

    GeometricShape readGeometricShape() {
        while(true) {
            System.out.println("Choose shape: cube, cylinder, sphere.");
            String shape = readString().toLowerCase();

            if(shape.equals("cube")) {
                System.out.print("Length: ");
                double length = readDouble();
                Cube ret = new Cube(length);
                return ret;
            }
            if(shape.equals("cylinder")) {
                System.out.print("Radius: ");
                double radius = readDouble();
                System.out.print("Height: ");
                double height = readDouble();
                Cylinder ret = new Cylinder(radius, height);
                return ret;
            }
            if(shape.equals("sphere")) {
                System.out.print("Radius: ");
                double radius = readDouble();
                Sphere ret = new Sphere(radius);
                return ret;
            }

        }
    }

    public void run() {
        while(true) {
            menu();
            String op = readString();

            System.out.format("You chose operation <%s>\n", op);

            switch(op.toLowerCase()) {
                case "add":
                    System.out.println("You chose to add a shape");
                    this.c.add(readGeometricShape());
                    break;
                case "solve":
                    for(GeometricShape x:this.c.FilterByVolume(25.0)) {
                        System.out.format("\t%s\n", x.toString());
                    }
                    break;
                case "exit":
                    return;
            }

        }
    }

}
