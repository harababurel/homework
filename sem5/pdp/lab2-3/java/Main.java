package pdp;

public class Main {
  public static void main(String[] args) {
    MatrixMultiplier mult = new MatrixMultiplier(4);
    Matrix A = new Matrix(500, 500, true);
    Matrix B = new Matrix(500, 500, true);

    System.out.println("Starting to do work");
    // Matrix seq_product = mult.SequentialProduct(A, B);
    Matrix threadpool_product = mult.ThreadPoolProduct(A, B);
    mult.Shutdown();
    System.out.println("Work finished");

    // System.out.println(A);
    // System.out.println(B);
    // System.out.println(seq_product);
    // System.out.println(threadpool_product);

    System.out.println("Done");
  }
}
