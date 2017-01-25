package ctrl;

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


public class MainCtrl {
    private Controller superCtrl;

    @FXML
    private ListView programStates;

    /* Initializes the controller class. This method is */
    /* automatically called */
    /* after the fxml file has been loaded. */
    @FXML
    private void initialize() {
        System.out.println("main window controller initialized");
    }

    public void update() {
        populatePrgStates();
    }

    private void populatePrgStates() {
        List <PrgState> programs = superCtrl.getRepo().getPrgList();
        ObservableList<String> shownItems = FXCollections.observableArrayList(
                programs.stream().map(p -> "Program #" + p.getID()).collect(Collectors.toList())
        );

        /* programStatesTitle.setText("Program States: " + programs.size()); */
        programStates.setItems(shownItems);
        programStates.refresh();
    }

    public void setSuperCtrl(Controller ctrl) {
        this.superCtrl = ctrl;
    }

}
