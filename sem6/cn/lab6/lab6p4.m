axis([0,3,0,5])
[x, y] = ginput(10)

P4 = polyfit(x, y, 10);

hold on

plot(x, y, '*')
plot([0:0.01:3], polyval(P4, [0:0.01:3]))
