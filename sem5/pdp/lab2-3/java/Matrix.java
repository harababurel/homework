package pdp;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class Matrix {
  private List<ArrayList<Integer>> vals;

  public Matrix(int n_rows, int n_cols, boolean random) {
    vals = new ArrayList<ArrayList<Integer>>(n_rows);
    for (int i = 0; i < n_rows; i++) {
      ArrayList<Integer> row = new ArrayList<>(n_cols);

      for (int j = 0; j < n_cols; j++) {
        if (random) {
          int min = 0;
          int max = 100000;
          int randomNum = ThreadLocalRandom.current().nextInt(min, max + 1);
          row.add(randomNum);
        } else {
          row.add(0);
        }
      }

      vals.add(row);
    }
  }

  public int n_rows() {
    return vals.size();
  }

  public int n_cols() {
    return vals.get(0).size();
  }

  public Integer get(int row, int col) {
    return vals.get(row).get(col);
  }

  public void set(int row, int col, Integer val) {
    vals.get(row).set(col, val);
  }

  @Override
  public String toString() {
    String ret = new String();

    for (int i = 0; i < n_rows(); i++) {
      ret += vals.get(i).toString() + "\n";
    }

    return ret;
  }
}
