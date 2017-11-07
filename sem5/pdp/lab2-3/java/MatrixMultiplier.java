package pdp;

import java.util.*;
import java.util.concurrent.*;

public class MatrixMultiplier {
  private int n_threads_;
  private ExecutorService executor_service_;

  public MatrixMultiplier(int n_threads) {
    n_threads_ = n_threads;
    executor_service_ = Executors.newFixedThreadPool(n_threads);
    // completion_service_ = new ExecutorCompletionService<>(executor_service_);
  }

  Matrix SequentialProduct(Matrix A, Matrix B) {
    Matrix ret = new Matrix(A.n_rows(), B.n_cols(), false);

    for (int i = 0; i < ret.n_rows(); i++) {
      for (int j = 0; j < ret.n_cols(); j++) {
        for (int k = 0; k < A.n_cols(); k++) {
          ret.set(i, j, ret.get(i, j) + A.get(i, k) * B.get(k, j));
        }
      }
    }

    return ret;
  }

  Matrix ThreadPoolProduct(Matrix A, Matrix B) {
    final Matrix ret = new Matrix(A.n_rows(), B.n_cols(), false);
    final Matrix A_ = A;
    final Matrix B_ = B;

    List<Future<Void>> futures = new ArrayList<>();

    for (int i = 0; i < ret.n_rows(); i++) {
      for (int j = 0; j < ret.n_cols(); j++) {
        final int i_ = i;
        final int j_ = j;
        Callable<Void> task =
            () -> {
              for (int k = 0; k < A_.n_cols(); k++) {
                ret.set(i_, j_, ret.get(i_, j_) + A_.get(i_, k) * B_.get(k, j_));
              }
              return null;
            };

        futures.add(executor_service_.submit(task));
      }
    }

    for (Future<Void> future : futures) {
      try {
        future.get();
      } catch (InterruptedException e) {
        System.out.println("InterruptedException");
      } catch (ExecutionException e) {
        System.out.println("ExecutionException");
      }
    }

    return ret;
  }

  public void Shutdown() {
    executor_service_.shutdown();
  }

  // private CompletionService completion_service_;
}
