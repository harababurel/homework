xs = [1:5]
ys = [22 23 25 30 28]

first_order = arrayfun(@(i) (ys(i) - ys(i-1)) / (xs(i) - xs(i-1)), 2:length(xs));

second_order = arrayfun(@(i) (first_order(i) - first_order(i-1)) / (xs(i+1) - xs(i-1)), 2:length(first_order));

function divided_differences = f(order, xs, ys)
  if order == 0
    divided_differences = arrayfun(@(i) (ys(i) - ys(i-1)) / (xs(i) - xs(i-1)), 2:length(xs))
  else
    divided_differences = arrayfun(@(i) (f(order-1, xs)(i) - first_order(i-1)) / (xs(i+1) - xs(i-1)),
                        2:length(first_order))
  endif
endfunction

a = @(j) f(0, xs, ys)(1:j)
n = @(j, x, xs) prod(ones(1, j-1) * x - xs(1:j-1))
N = @(x, xs, a) a(j) * arrayfun(@(j) n(j, x, xs), 1:size(xs, 2))'

hold on

# f(0, xs, ys)

plot(xs, ys, '*')

rng=[1:0.1:5];
plot(rng, arrayfun(@(x) N(x, xs, a), rng))

# printf("2.5 pounds of fertilizer => expect %.2f pounds of potatoes\n", N(2.5, xs, ys));
