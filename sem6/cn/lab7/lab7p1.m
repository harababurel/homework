hold on
set(gca, "linewidth", 4, "fontsize", 12)

f = @(x) 2 ./ (1+x.^2)
a = 0
b = 1


# actual function
rng=[a:0.1:b];
plot(rng, f(rng), "linewidth", 4)

# Trapezium aka first order Lagrange
plot([a,a,b,b], [0,f(a),f(b),0], "linewidth", 4)

# area of trapezium
approximation = (f(b) + f(a)) * (b-a) / 2

# Simpson's formula aka second order Lagrange (without error aka R_2(f)
approximation = (b-a) / 6 * (f(a) + 4 * f((a+b)/2) + f(b))

# Repeated trapezium approximation
printf("Repeated trapezium\n")
printf("approximation: %.10f\n", rep_trapezium(a, b, f, 1000))
printf("expected:      %.10f\n", pi/2)




