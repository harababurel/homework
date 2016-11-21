package view;
import ctrl.*;

public class RunExample extends Command {
    private Controller ctrl;

    public RunExample(String key, String description, Controller ctrl) {
        super(key, description);
        this.ctrl = ctrl;
    }

    @Override
    public void execute() {
        try {
            ctrl.allStep();
        } catch(Exception e) {
            System.err.println("Errors occurred when running the program.");
            System.exit(1);
        }
    }
}
