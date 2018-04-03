x = -3:0.4:3;
y = sin(x);

P4 = polyfit(x, y, 4);

hold on

plot(x, y, '*')
plot(x, polyval(P4, x))

for i=1:m
  plot([x(i), x(i)], [y(i), polyval(P4, x(i))])
endfor
