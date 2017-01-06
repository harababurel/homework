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

public class GraphicalMenu extends Application {
    private Map <String, Command> commands;

    public GraphicalMenu() {
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

        ObservableList <String> programs = FXCollections.observableArrayList("pula", "mea");

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
                actionTarget.setFill(Color.FIREBRICK);
                actionTarget.setText("Button pressed.");
            }
        });

        primaryStage.setScene(scene);
        primaryStage.show();

    }

    public static void run(String[] args) {
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
