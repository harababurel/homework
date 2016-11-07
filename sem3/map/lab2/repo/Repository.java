package repo;
import models.*;

public class Repository implements IRepository {
    private MyList <PrgState> states;

    public Repository() {
        this.states = new MyList <PrgState>();
    }

    public PrgState getCurrentPrg() {
        // LinkedList implements getLast(), but this is
        // a more general List.
        return this.states.get(this.states.size()-1);
    }
}
