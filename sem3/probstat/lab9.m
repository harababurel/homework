% z(1-alfa/2) = inv de quantile = norminv(1-alfa/2, 0, 1)
%                                                   ^ parametri care tin de
%                                                   normala
% t(1-alfa/2) = tinv(1-alfa/2, n-1)

% PROBLEM 1

confidence_level = input('PROBLEM 1: confidence level = '); % 1 - alfa
alpha = 1 - confidence_level;

xs = [ 7,  7,  4, 5,  9,  9, ...
       4, 12,  8, 1,  8,  7, ...
       3, 13,  2, 1, 17,  7, ...
      12,  5,  6, 2,  1, 13, ...
      14, 10,  2, 4,  9, 11, ...
       3,  5, 12, 6, 10,  7];

avg = mean(xs);
sigma = 5;

q1 = norminv(1-alpha/2, 0, 1); % quantiles
q2 = norminv(  alpha/2, 0, 1);
n = length(xs);                % sample size

miu_lo = avg - sigma / sqrt(n) * q1; % confidence interval
miu_hi = avg - sigma / sqrt(n) * q2;

fprintf('confidence interval for the mean (sigma known): (%3.4f, %3.4f)\n', miu_lo, miu_hi);

clear all;

% PROBLEMA 2

%xs = [99.8, 99.8, 99.9, 99.9, 99.9, 99.9, 99.9, 98.0, 98.0, 98.0 100.1, 100.1, 100.1, 100.1, 100.5, 100.5, 100.0, 100.0, 100.2, 100.2];

xs = [99.8 * ones(1, 2), 99.9 * ones(1, 5), 98.0 * ones(1, 3), 100.1 * ones(1, 4), ...
      100.5 * ones(1, 2), 100.0 * ones(1, 2), 100.2 * ones(1, 2)];

confidence_level = input('PROBLEM 2: confidence level = '); % 1 - alfa
alpha = 1 - confidence_level;

avg = mean(xs);
n = length(xs);

s = std(xs);

t1 = tinv(1-alpha/2, n-1);
t2 = tinv(  alpha/2, n-1);

miu_lo = avg - s / sqrt(n) * t1;
miu_hi = avg - s / sqrt(n) * t2;

fprintf('confidence interval for the mean (sigma unknown): (%3.4f, %3.4f)\n', miu_lo, miu_hi);

% PROBLEMA 3

clear all;

xs = [1.48 1.26 1.52 1.56 1.48 1.46 ...
      1.30 1.28 1.43 1.43 1.55 1.57 ...
      1.51 1.53 1.68 1.37 1.47 1.61 ...
      1.49 1.43 1.64 1.51 1.60 1.65 ...
      1.60 1.64 1.51 1.51 1.53 1.74];

n = length(xs);

confidence_level = input('PROBLEM 3: confidence level = '); % 1 - alfa
alpha = 1 - confidence_level;

s_sq = var(xs);

chi2_quantile1 = chi2inv(1 - alpha/2, n-1);
chi2_quantile2 = chi2inv(    alpha/2, n-1);

miu_lo = (n-1) * s_sq / chi2_quantile1;
miu_hi = (n-1) * s_sq / chi2_quantile2;

fprintf('confidence interval for the variance: (%3.4f, %3.4f)\n', miu_lo, miu_hi);
fprintf('confidence interval for the std deviation: (%3.4f, %3.4f)\n', sqrt(miu_lo), sqrt(miu_hi));



