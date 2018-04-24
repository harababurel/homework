f = @(x) exp(-x.^2)

printf("%.10f\n", rep_rectangle(1, 1.5, f, 1));
printf("%.10f\n", rep_rectangle(1, 1.5, f, 100));
printf("%.10f\n", rep_rectangle(1, 1.5, f, 150));
printf("%.10f\n", rep_rectangle(1, 1.5, f, 1e6));
