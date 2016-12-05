confidence_level = input('confidence level = ');
alpha = 1 - confidence_level;

% CASE 1: sigma1 = sigma2

xs = [22.4 21.7 ...
      24.5 23.4 ...
      21.6 23.3 ...
      22.4 21.6 ...
      24.8 20.0];

ys = [17.7 14.8 ...
      19.6 19.6 ...
      12.1 14.8 ...
      15.4 12.6 ...
      14.0 12.2];
  
n1 = length(xs);
n2 = length(ys);

avg1 = mean(xs);
avg2 = mean(ys);

var1 = var(xs);
var2 = var(ys);

s1_sq = var1;
s2_sq = var2;

sp_sq = ((n1-1) * s1_sq + (n2-1) * s2_sq) / (n1+n2-2);
sp = sqrt(sp_sq);

t = tinv(1 - alpha/2, n1+n2-2);

miu_diff_lo = avg1 - avg2 - t * sp * sqrt(1/n1 + 1/n2);
miu_diff_hi = avg1 - avg2 + t * sp * sqrt(1/n1 + 1/n2);

fprintf('confidence interval for the mean difference (sigma1  =  sigma2): (%3.5f, %3.5f)\n', miu_diff_lo, miu_diff_hi);

% CASE 2: sigma1 =/= sigma2

c = (s1_sq/n1) / (s1_sq/n1 + s2_sq/n2);
n = 1 / (c^2 / (n1-1) + (1-c)^2 / (n2-1));

t2 = tinv(1 - alpha/2, n);

miu_diff_lo2 = avg1 - avg2 - t2 * sqrt(s1_sq/n1 + s2_sq/n2);
miu_diff_hi2 = avg1 - avg2 + t2 * sqrt(s1_sq/n1 + s2_sq/n2);

fprintf('confidence interval for the mean difference (sigma1 =/= sigma2): (%3.5f, %3.5f)\n', miu_diff_lo2, miu_diff_hi2);