
l = @(j, x, xs) prod(ones(1, j-1) * x - xs(1:j-1)) * \
                prod(ones(1, size(xs, 2)-j) * x - xs(j+1:end)) / \
                prod(ones(1, j-1) * xs(j) - xs(1:j-1)) / \
                prod(ones(1, size(xs, 2)-j) * xs(j) - xs(j+1:end))

L = @(x, xs, ys) ys * arrayfun(@(j) l(j, x, xs), 1:size(xs, 2))'

# printf("%.2f\n", L(1955, x, y));

hold on

xs = [1930 1940 1950 1960 1970 1980]
ys = [123203 131669 150697 179323 203212 226505]

plot(xs, ys, '*')

years = [1930:1:1980]
plot(years, arrayfun(@(x) L(x, xs, ys), years))
# fplot(@(x) L(x, xs, ys))

L(1930, xs, ys)


