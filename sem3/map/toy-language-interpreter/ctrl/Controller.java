package ctrl;
import models.*;
import repo.*;
import java.io.*;
import java.util.*;
import java.util.stream.*;
import java.util.concurrent.*;

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

public class Controller {
    private IRepository r;
    private ExecutorService executor;

    public Controller() {
        this.r = new Repository("");
    }

    public Controller(IRepository r) {
        this.r = r;
    }

    List <PrgState> removeCompletedPrg(List <PrgState> prgList) {
        return prgList.stream()
                .filter(p -> p.isNotCompleted())
                .collect(Collectors.toList());
    }

    public IRepository getRepo() {
        return this.r;
    }

    /* No longer works for concurrent programs
    /* MyDictionary <Integer,Integer> conservativeGarbageCollector(Collection <Integer> symTableValues, */
    /*                                                   MyIHeap heap) { */
    /*     MyDictionary <Integer, Integer> new_content = new MyDictionary <Integer, Integer>(); */
    /*     for(Map.Entry <Integer, Integer> x:heap.entrySet().stream() */
    /*                                                       .filter(e -> symTableValues.contains(e.getKey())) */
    /*                                                       .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)) */
    /*                                                       .entrySet()) */
    /*         new_content.put(x.getKey(), x.getValue()); */
    /*     return new_content; */
    /* } */

    /*
    public void allStep() throws Exception {
        PrgState state = r.getCurrentState();

        // initial state
        System.out.println(state.toString());
        try {
            this.r.logPrgStateExec();
        } catch(IOException e) { ; }

        while(!state.getExeStack().isEmpty()) {
            state.oneStep();
            state.getHeap().setContent(conservativeGarbageCollector(
                        state.getSymTable().values(),
                        state.getHeap()));
            System.out.println(state.toString());
            try {
                //this.r.logPrgStateExec(state);
                this.r.serialize(File.createTempFile("toy_language_interpreter_", ".ser").getAbsolutePath());
            } catch(IOException e) {
                System.out.println(e);
            }
        }
    }
    */

    public void allStep() {
        executor = Executors.newFixedThreadPool(2);
        while(true) {

            // remove completed programs
            List <PrgState> prgList = removeCompletedPrg(r.getPrgList());

            if(prgList.size() == 0)
                break;  // complete execution of all threads

            oneStepForAllPrg(prgList);
        }
        executor.shutdownNow();
    }

    public void allStepGUI() {
        executor = Executors.newFixedThreadPool(2);

        // remove completed programs
        List <PrgState> prgList = removeCompletedPrg(r.getPrgList());

        if(prgList.size() == 0) {
            // display a window message saying that the execution terminates
            executor.shutdownNow();
        }
        else {
            oneStepForAllPrg(prgList);
            executor.shutdownNow();
        }
    }


    public void oneStepForAllPrg(List <PrgState> prgList) {
        // Log each program state

        System.out.printf("taking one step for each of the %d programs.\n", prgList.size());

        prgList.forEach(prg -> {
            try {
                System.out.printf("logging prgstate %d\n", prg.getID());
                r.logPrgStateExec(prg);
            } catch(Exception e) {
                e.printStackTrace();
            }
        });

        List <Callable <PrgState>> callList = prgList.stream()
            .map(p -> (Callable <PrgState>) () -> p.oneStep())
            .collect(Collectors.toList());

        try {
            List <PrgState> newPrgList = executor.invokeAll(callList).stream()
                .map(future -> {
                    try {
                        return future.get();
                    } catch(Exception e) {
                        e.printStackTrace();
                    }
                    return null;
                })
                .filter(p -> p != null)
                .collect(Collectors.toList());

            for(PrgState x:newPrgList) {
                prgList.add(x);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }

        //prgList.addAll(newPrgList);

        prgList.forEach(prg -> {
            try {
                /* System.out.println(prg.getID()); */
                r.logPrgStateExec(prg);
            } catch(Exception e) {
                e.printStackTrace();
            }
        });
        r.setPrgList(prgList);
    }

}
