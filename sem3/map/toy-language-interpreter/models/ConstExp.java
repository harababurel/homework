package models;

public class ConstExp extends Exp {
    private int value;

    public ConstExp(int value) {
        this.value = value;
    }

    @Override
    public int eval(MyIDictionary <String, Integer> symTable,
                    MyIHeap heap) {
        return this.value;
    }

    @Override
    public String toString() {
        return Integer.toString(this.value);
    }
}
