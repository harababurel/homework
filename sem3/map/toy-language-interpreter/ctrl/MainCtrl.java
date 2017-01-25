package ctrl;

import models.*;

import java.util.*;
import java.util.Collections;
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
import javafx.scene.control.Alert;
import javafx.beans.property.SimpleStringProperty;

/* import static java.util.stream.Collectors.toList; */


public class MainCtrl {
    private Controller superCtrl;

    @FXML
    private ListView <String> prgStatesView;

    @FXML
    private ListView <String> exeStackView;

    @FXML
    private TableView <Map.Entry <Integer, Integer>> heapTableView;

    @FXML
    private TableColumn <Map.Entry <Integer, Integer>, String> heapTableAddressColumn;

    @FXML
    private TableColumn <Map.Entry <Integer, Integer>, String> heapTableValueColumn;

    @FXML
    private TableView <Map.Entry <String, Integer>> symTableView;

    @FXML
    private TableColumn <Map.Entry <String, Integer>, String> symTableVarnameColumn;

    @FXML
    private TableColumn <Map.Entry <String, Integer>, String> symTableValueColumn;


    @FXML
    private Button runOneStepButton;

    /* Initializes the controller class. This method is */
    /* automatically called */
    /* after the fxml file has been loaded. */
    @FXML
    private void initialize() {
        System.out.println("main window controller initialized");

        heapTableAddressColumn.setCellValueFactory(
            p -> new SimpleStringProperty(p.getValue().getKey() + ""));

        heapTableValueColumn.setCellValueFactory(
            p -> new SimpleStringProperty(p.getValue().getValue() + ""));

        symTableVarnameColumn.setCellValueFactory(
            p -> new SimpleStringProperty(p.getValue().getKey() + ""));

        symTableValueColumn.setCellValueFactory(
            p -> new SimpleStringProperty(p.getValue().getValue() + ""));

    }

    public void update() {
        populatePrgStates();

        int index = prgStatesView.getFocusModel().getFocusedIndex();

        if (index < 0 && !prgStatesView.getItems().isEmpty()) {
            prgStatesView.getFocusModel().focus(0);
            index = 0;
        }

        populateHeapTable(index);
        populateSymTable(index);
        populateExeStack(index);
    }

    private void populatePrgStates() {
        List <PrgState> programs = superCtrl.getRepo().getPrgList();

        ObservableList<String> shownItems = FXCollections.observableArrayList(
                programs.stream()
                        .map(p -> "Program #" + p.getID())
                        .collect(Collectors.toList())
        );

        /* programStatesTitle.setText("Program States: " + programs.size()); */
        this.prgStatesView.setItems(shownItems);
        this.prgStatesView.refresh();
    }

    private void populateExeStack(int index) {
        ObservableList <String> shownItems;

        if (index == -1) {
            shownItems = FXCollections.observableArrayList(new ArrayList<>());
        } else {
            List <PrgState> programs = superCtrl.getRepo().getPrgList();
            PrgState program = programs.get(index);

            shownItems = FXCollections.observableArrayList(
                    program.getExeStack().getStack()
                                         .stream()
                                         .map(p -> p.toString())
                                         .collect(Collectors.toList()));

            Collections.reverse(shownItems);
        }

        exeStackView.setItems(shownItems);
        exeStackView.refresh();
    }



    private void populateHeapTable(int index) {
        ObservableList <Map.Entry<Integer, Integer>> shownItems;

        if (index == -1) {
            shownItems = FXCollections.observableArrayList(new ArrayList<>());
        } else {
            List <PrgState> programs = superCtrl.getRepo().getPrgList();
            PrgState program = programs.get(index);
            /* List <Map.Entry <Integer, Integer>> elements = new ArrayList(program.getHeap().entrySet()); */

            shownItems = FXCollections.observableArrayList(program.getHeap().entrySet());
        }

        /* for(Map.Entry <Integer, Integer> entry:shownItems) */
        /*     System.out.println(entry); */

        this.heapTableView.setItems(shownItems);
        this.heapTableView.refresh();
    }

    private void populateSymTable(int index) {
        ObservableList <Map.Entry<String, Integer>> shownItems;

        if (index == -1) {
            shownItems = FXCollections.observableArrayList(new ArrayList<>());
        } else {
            List <PrgState> programs = superCtrl.getRepo().getPrgList();
            PrgState program = programs.get(index);

            shownItems = FXCollections.observableArrayList(program.getSymTable().entrySet());
        }

        this.symTableView.setItems(shownItems);
        this.symTableView.refresh();
    }


    @FXML
    private void handleRunOneStep() {
        System.out.println("one step button was pressed.");
        try {
            superCtrl.oneStepGUI();
        } catch (Exception e) {
            showMessage(e.toString());
        }

        update();
    }

    static void showMessage(String text) {
        Alert message = new Alert(Alert.AlertType.INFORMATION);
        message.setTitle("Message");
        message.setContentText(text);
        message.showAndWait();
    }

    public void setSuperCtrl(Controller ctrl) {
        this.superCtrl = ctrl;
    }

}
