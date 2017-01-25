package view;
import models.*;
import repo.*;
import ctrl.*;
import java.util.*;

import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.layout.StackPane;
import javafx.stage.Stage;
import javafx.geometry.Pos;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.geometry.Insets;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;
import javafx.scene.control.ListView;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;


public class GraphicalMenu extends Application {
    private static Map <String, Command> commands = new HashMap <String, Command>();

    public GraphicalMenu() {
        if(commands == null) {
            commands = new HashMap <String, Command>();
        }
    }

    public static void addCommand(Command c) {
        commands.put(c.getKey(), c);
    }


    private void printMenu() {
        for(Command c:commands.values()) {
            String line = String.format("%4s: %s", c.getKey(), c.getDescription());
            System.out.println(line);
        }
    }

    @Override
    public void start(Stage primaryStage) {
        //do stuff
        primaryStage.setTitle("Toy Language Interpreter");

        GridPane grid = new GridPane();
        grid.setAlignment(Pos.CENTER);
        grid.setHgap(10);
        grid.setVgap(10);
        grid.setPadding(new Insets(25, 25, 25, 25));

        Scene scene = new Scene(grid, 300, 200);

        // create some objects
        Text sceneTitle = new Text("Initial Program");
        sceneTitle.setFont(Font.font("Tahoma", FontWeight.NORMAL, 20));

        Button btn = new Button("Run");
        HBox hbbtn = new HBox(10);
        hbbtn.setAlignment(Pos.BOTTOM_RIGHT);
        hbbtn.getChildren().add(btn);

        ObservableList <String> programs = FXCollections.observableArrayList();

        // add the initial program entries
        for(Map.Entry <String, Command> e:commands.entrySet())
            programs.add(e.getValue().getDescription());

        ListView <String> programList = new ListView <String>(programs);

        // add some objects
        grid.add(programList, 1, 2);
        grid.add(hbbtn, 1, 3);

        /* grid.setGridLinesVisible(true); */
        grid.add(sceneTitle, 0, 0, 2, 1);

        final Text actionTarget = new Text();
        grid.add(actionTarget, 1, 6);   // not shown yet

        btn.setOnAction(new EventHandler <ActionEvent>() {
            @Override
            public void handle(ActionEvent e) {
                /* actionTarget.setFill(Color.FIREBRICK); */
                /* actionTarget.setText("Button pressed."); */

                Parent root;
                try {
                    root = FXMLLoader.load(getClass().getClassLoader().getResource("RunProgram.fxml"));
                    Stage stage = new Stage();
                    stage.setTitle("My New Stage Title");
                    stage.setScene(new Scene(root, 450, 450));
                    stage.show();
                    // Hide this current window (if this is what you want)
                    ((Node)(e.getSource())).getScene().getWindow().hide();
                }
                catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        });

        primaryStage.setScene(scene);
        primaryStage.show();

    }

    public static void run(String[] args) {
        System.out.println(commands.values().size());
        System.out.println("will launch now");
        launch(args);

        /*
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
        */
    }

}
