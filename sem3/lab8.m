x = [20*ones(1,2), 21, 22*ones(1,3), 23*ones(1,6), 24*ones(1,5), 25*ones(1,9), 26*ones(1,2), 27*ones(1,2)];
y = [75*ones(1,3), 76*ones(1,2), 77*ones(1,2), 78*ones(1,5), 79*ones(1,8), 80*ones(1,8), 81, 82];

meanx = mean(x);
meany = mean(y);

varx = var(x, 1);
vary = var(y, 1);

covxy = cov(x, y, 1);

corrcoefxy = corrcoef(x, y);

fprintf('meanx = %3.3f\n', meanx);
fprintf('meany = %3.3f\n', meany);


fprintf('varx = %3.3f\n', varx);
fprintf('vary = %3.3f\n', vary);

fprintf('covxy = %3.3f\n', covxy(1,2));
fprintf('corrcoefxy = %3.3f\n', corrcoefxy(1,2));

scatter(x, y);
hold on;


xregr = min(x)-2:max(x)+2;
yregr = meany + corrcoefxy(1,2) * sqrt(vary) / sqrt(varx) * (xregr - meanx);

plot(xregr, yregr, '-');


