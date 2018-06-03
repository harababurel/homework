printf("Find the smallest n for which repeating (trapezium|simpson) n times yields a desired precision.\n")

function result = find_first_n(precision, method)
  expected = 0.6362943688583;
  f = @(x) x .* log(x);

  for n = 1:1e8
    approx = method(1, 2, f, n);
    error = abs(expected - approx);
    # printf("Repeating the trapezium %2d times yields:\n", n);
    # printf("\tapprox: %.10f\n", approx);
    # printf("\terror:  %.10f\n", error);

    if error < precision
      result = n;
      return;
    endif
  endfor
endfunction

precision = 10^(-7);

printf("Trapezium: found the desired precision using n = %d.\n", \
  find_first_n(precision, @rep_trapezium));
printf("Simpson: found the desired precision using n = %d.\n", \
  find_first_n(precision, @rep_simpson));
