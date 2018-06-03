l1 = @(x) x
l2 = @(x) 3/2 * x.^2 - 1/2
l3 = @(x) 5/2 * x.^3 - 3/2 * x
l4 = @(x) 35/8 * x.^5 - 15/4 * x.^2 + 3/8

range = [0:0.01:1];

subplot(2, 2, 1)
plot(range, l1(range));
title("l1(x) for x in [0..1]")

subplot(2, 2, 2)
plot(range, l2(range));
title("l2(x) for x in [0..1]")

subplot(2, 2, 3)
plot(range, l3(range));
title("l3(x) for x in [0..1]")

subplot(2, 2, 4)
plot(range, l4(range));
title("l4(x) for x in [0..1]")
