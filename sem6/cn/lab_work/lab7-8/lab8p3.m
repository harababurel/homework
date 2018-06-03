hold on;
set(gca, "linewidth", 4, "fontsize", 12);

f = @(x) 100 ./ (x.^2) .* sin(10./x)
precision = 1e-9;
a = 1
b = 3

expected = -1.4260247818;

printf("expected:\t\t\t%.10f\n", expected);
printf("simpson(precision = %f):  %.10f\n", precision, approx_with_precision(f, a, b, precision, @rep_simpson))
printf("simpson(n = %3d):\t\t%.10f\n", 100, rep_simpson(a, b, f, 100));
printf("simpson(n = %3d):\t\t%.10f\n", 150, rep_simpson(a, b, f, 150));

# Plots

axis off
plot([1, 3], [0, 0], 'k', "linewidth", 4)
plot([1, 1], [-60, 70], 'k', "linewidth", 4)

step = 1e-2
x = [a:step:b];
plot(x, f(x), "linewidth", 4);

function plot_rectangles(f, a, b, n)
  points = linspace(a, b, n+1);
  lows = points(1:end-1);
  highs = points(2:end);

  for i=1:length(lows)
    printf("%d %d\n", lows(i), highs(i))
    mid = (lows(i) + highs(i)) / 2
    height = f(mid)

    # current rectangle:
    # (lows(i), 0) -> (lows(i), height) -> (highs(i), height) -> (highs(i), 0)
    plot([lows(i), lows(i), highs(i), highs(i)],
        [0, height, height, 0], "linewidth", 2)
  endfor
endfunction

plot_rectangles(f, a, b, 50)
