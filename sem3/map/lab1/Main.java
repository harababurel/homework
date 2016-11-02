/* 7. Se da o colectie de mai multe obiecte
 * avind forme de cuburi, sfere si cilindrii.
 * Sa se afiseze obiectele avind volumul mai mare
 * decit 25cm3.
 */

import controller.Controller;
import model.Cube;
import model.Cylinder;
import model.Sphere;
import repository.InMemRepository;
import repository.Repository;
import view.UI;

public class Main {
    public static void main(String[] args) {
        System.out.println("asdf");

        InMemRepository r = new InMemRepository(100);
        Controller c = new Controller(r);
        UI ui = new UI(c);

        Cube shape1 = new Cube(10);
        Cube shape2 = new Cube(123.5);
        Cylinder shape3 = new Cylinder(1.5, 0.2);
        Cylinder shape4 = new Cylinder(15, 3.14);
        Sphere shape5 = new Sphere(2.2);
        Sphere shape6 = new Sphere(3.14);

        ui.c.add(shape1);
        ui.c.add(shape2);
        ui.c.add(shape3);
        ui.c.add(shape4);
        ui.c.add(shape5);
        ui.c.add(shape6);

        ui.run();
    }
}
