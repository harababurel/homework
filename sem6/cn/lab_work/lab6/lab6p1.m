x = [1 2 3 4 5 6 7]
y = [13 15 20 14 15 13 10]

m = length(x);

a = (m * x * y' - sum(x) * sum(y)) / (m * x * x' - sum(x)^2)
b = (x*x' * sum(y) - x*y' * sum(x)) / (m * x * x' - sum(x)^2)
f = @(x) a * x + b;

hold on

plot(x, y, '*')
plot([0, 8], f([0, 8]))


for i=1:m
  plot([x(i), x(i)], [y(i), f(x(i))])
endfor

err = norm(y - f(x)) ^ 2
