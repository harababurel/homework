x = [0 10 20 30 40 60 80 100];
y = [0.0061 0.0123 0.0234 0.0424 0.0738 0.1992 0.4736 1.0133];

m = length(x);


P1 = polyfit(x, y, 1);
P2 = polyfit(x, y, 2);
P3 = polyfit(x, y, 3);
P8 = polyfit(x, y, 8);

T = 45
expected = 0.095848

printf("first degree polynomial: prediction = %.2f, error = %.2f\n",
  polyval(P1, T),
  abs(expected - polyval(P1, T)))

printf("second degree polynomial: prediction = %.2f, error = %.2f\n",
  polyval(P2, T),
  abs(expected - polyval(P2, T)))

printf("third degree polynomial: prediction = %.2f, error = %.2f\n",
  polyval(P3, T),
  abs(expected - polyval(P3, T)))

hold on

plot(x, y, '*')

plot([0, 100], polyval(P1, [0, 100]))
plot([0:0.1:100], polyval(P2, [0:0.1:100]))
plot([0:0.1:100], polyval(P3, [0:0.1:100]))
plot([0:0.1:100], polyval(P8, [0:0.1:100]))

for i=1:m
  plot([x(i), x(i)], [y(i), polyval(P1, x(i))])
endfor

#err = norm(y - f(x)) ^ 2
