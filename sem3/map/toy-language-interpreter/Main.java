import models.*;
import repo.*;
import ctrl.*;
import view.*;
import view.TextMenu.*;
import view.GraphicalMenu.*;
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
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.layout.AnchorPane;
import javafx.scene.paint.Color;
import javafx.scene.control.ListView;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Group;
import javafx.scene.shape.Rectangle;
import javafx.scene.shape.Circle;

public class Main extends Application {

    @Override
    public void start (Stage prgListStage) {
        Stage mainStage = new Stage();
        Controller ctrl = new Controller();
        /*
        Group root = new Group();
        Scene scene = new Scene(root, 500, 500, Color.PINK);
        stage.setTitle("Welcome to JavaFX!");

        Rectangle r = new Rectangle(25, 25, 50, 50);
        r.setFill(Color.BLUE);
        root.getChildren().add(r);

        Circle c = new Circle(200,200,50, Color.web("blue", 0.5f));
        root.getChildren().add(c);

        stage.setScene(scene);
        stage.show();
        */

        try {
            FXMLLoader prgListLoader = new FXMLLoader();
            prgListLoader.setLocation(Main.class.getResource("fxml/PrgList.fxml"));
            AnchorPane prgListLayout = (AnchorPane) prgListLoader.load();
            PrgListCtrl prgListCtrl = prgListLoader.getController();
            prgListCtrl.setSuperCtrl(ctrl);
            Scene prgListScene = new Scene(prgListLayout);
            prgListStage.setScene(prgListScene);
            prgListStage.show();


            FXMLLoader mainLoader = new FXMLLoader();
            mainLoader.setLocation(Main.class.getResource("fxml/Main.fxml"));
            AnchorPane mainLayout = (AnchorPane) mainLoader.load();
            MainCtrl mainCtrl = mainLoader.getController();
            mainCtrl.setSuperCtrl(ctrl);
            Scene mainScene = new Scene(mainLayout);
            mainStage.setScene(mainScene);

            prgListCtrl.setMainCtrl(mainCtrl);
            prgListCtrl.setMainStage(mainStage);

            //mainStage.show();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    public static void main(String[] args) {
        /* GraphicalMenu menu = new GraphicalMenu(); */
        /* TextMenu menu = new TextMenu(); */

        launch(args);

        /*
        String logFilePath = "main.log";


        GraphicalMenu.addCommand(new RunExample("1", "Lab2ex1", TextMenu.generateController(logFilePath, TextMenu.generateExample1())));
        GraphicalMenu.addCommand(new RunExample("2", "Lab2ex2", TextMenu.generateController(logFilePath, TextMenu.generateExample2())));
        GraphicalMenu.addCommand(new RunExample("3", "Lab2ex2", TextMenu.generateController(logFilePath, TextMenu.generateExample3())));
        GraphicalMenu.addCommand(new RunExample("4", "Lab5ex1", TextMenu.generateController(logFilePath, TextMenu.generateExample4())));
        GraphicalMenu.addCommand(new RunExample("5", "Lab5ex2", TextMenu.generateController(logFilePath, TextMenu.generateExample5())));
        GraphicalMenu.addCommand(new RunExample("6", "Lab6ex1", TextMenu.generateController(logFilePath, TextMenu.generateExample6())));
        GraphicalMenu.addCommand(new RunExample("7", "Lab6ex2", TextMenu.generateController(logFilePath, TextMenu.generateExample7())));
        GraphicalMenu.addCommand(new RunExample("8", "Lab8ex1", TextMenu.generateController(logFilePath, TextMenu.generateExample8())));
        GraphicalMenu.addCommand(new DeserializeCommand());

        GraphicalMenu.run(args);
        */
    }
}
