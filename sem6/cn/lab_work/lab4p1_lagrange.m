l = @(j, x, xs) prod(ones(1, j-1) * x - xs(1:j-1)) * \
                prod(ones(1, size(xs, 2)-j) * x - xs(j+1:end)) / \
                prod(ones(1, j-1) * xs(j) - xs(1:j-1)) / \
                prod(ones(1, size(xs, 2)-j) * xs(j) - xs(j+1:end))

L = @(x, xs, ys) ys * arrayfun(@(j) l(j, x, xs), 1:size(xs, 2))'

hold on

xs = [1:5];
ys = [22 23 25 30 28];

plot(xs, ys, '*')

rng=[1:0.1:5];
plot(rng, arrayfun(@(x) L(x, xs, ys), rng))

printf("2.5 pounds of fertilizer => expect %.2f pounds of potatoes\n", L(2.5, xs, ys));
