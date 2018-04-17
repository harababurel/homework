H = @(p, r, pieces) 60 * r / (r^2 - p^2) * \
  rep_trapezium(0, 2*pi, @(x) (1 - (p/r)^2 * sin(x)).^0.5, pieces)

r = 110
p = 75

H(p, r, 1)
H(p, r, 5)
H(p, r, 1000)
printf("Expected: 6.3131\n")
