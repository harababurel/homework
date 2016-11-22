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
            System.err.println(e.toString());
            System.exit(1);
        }
    }
}
