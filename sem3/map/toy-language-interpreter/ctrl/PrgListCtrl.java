package ctrl;

import view.*;
import models.*;

import java.util.*;
import java.util.stream.Collectors;
import javafx.fxml.FXML;

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
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Group;
import javafx.scene.shape.Rectangle;
import javafx.scene.shape.Circle;



public class PrgListCtrl {
    private Controller superCtrl;
    private MainCtrl mainCtrl;
    private Stage mainStage;

    @FXML
    private ListView prgStatesView;

    @FXML
    private Button runButton;

    /* Initializes the controller class. This method is */
    /* automatically called */
    /* after the fxml file has been loaded. */
    @FXML
    private void initialize() {
        populatePrgList();
    }

    private void populatePrgList() {
        List <IStmt> programs = new ArrayList <IStmt>();
        IStmt program = null;

        programs.add(TextMenu.generateExample1());
        programs.add(TextMenu.generateExample2());
        programs.add(TextMenu.generateExample3());
        programs.add(TextMenu.generateExample4());
        programs.add(TextMenu.generateExample5());
        programs.add(TextMenu.generateExample6());
        programs.add(TextMenu.generateExample7());
        programs.add(TextMenu.generateExample8());

        ObservableList<String> shownItems = FXCollections.observableArrayList(
            programs.stream().map(p -> p.toString()).collect(Collectors.toList())
        );

        prgStatesView.setItems(shownItems);
    }


    @FXML
    private void handleRunButton() {
        System.out.println("run button was clicked");

        int chosenProgramIndex = prgStatesView.getSelectionModel().getSelectedIndex() + 1;
        IStmt chosenProgram = null;
        switch(chosenProgramIndex) {
            case 1: chosenProgram = TextMenu.generateExample1(); break;
            case 2: chosenProgram = TextMenu.generateExample2(); break;
            case 3: chosenProgram = TextMenu.generateExample3(); break;
            case 4: chosenProgram = TextMenu.generateExample4(); break;
            case 5: chosenProgram = TextMenu.generateExample5(); break;
            case 6: chosenProgram = TextMenu.generateExample6(); break;
            case 7: chosenProgram = TextMenu.generateExample7(); break;
            case 8: chosenProgram = TextMenu.generateExample8(); break;
        }

        this.superCtrl.getRepo().setPrgList(
                Arrays.asList(new PrgState(chosenProgram)));

        this.mainCtrl.update();
        mainStage.show();
    }

    public void setSuperCtrl(Controller ctrl) {
        this.superCtrl = ctrl;
    }

    public void setMainCtrl(MainCtrl mainCtrl) {
        this.mainCtrl = mainCtrl;
    }


    public void setMainStage(Stage mainStage) {
        this.mainStage = mainStage;
    }



}
