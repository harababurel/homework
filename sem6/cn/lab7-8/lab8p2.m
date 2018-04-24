precision = 10^(-5);
f = @(x) 2 ./ (1 + x .^ 2)
a = 0
b = 1

printf("Trapezium: %.10f\n", approx_with_precision(f, a, b, precision, @rep_trapezium));
printf("Simpson:   %.10f\n", approx_with_precision(f, a, b, precision, @rep_simpson));
printf("expected:  %.10f\n", pi/2);
