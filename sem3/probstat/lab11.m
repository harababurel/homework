%%% PROBLEM 1 %%%
% sample data
X = [ 7,  7,  4, 5,  9,  9, ...
      4, 12,  8, 1,  8,  7, ...
      3, 13,  2, 1, 17,  7, ...
     12,  5,  6, 2,  1, 13, ...
     14, 10,  2, 4,  9, 11, ...
      3,  5, 12, 6, 10,  7];

alpha = input('significance level = ');

fprintf('H0 miu = 9 (miu > 9)\n')
fprintf('H1 miu < 9 (left-tailed test)\n')
fprintf('\n')

miu0 = 9;  % test value
sigma = 5; % sigma known

% Test Statistic = Z %isin; N(0, 1)

% ztest(X, miu0, sigma, significance level (default 0.05), tail);
%        { -1 or 'left'
% tail = {  0 (default)
%        { +1 or 'right'
% returns [h, p, ci, zval]
% h    = 0 or 1 (test failed or succeded <=> h0 or h1 is true)
% p    = p-value
% ci   = confidence interval
% zval = observed value of the test statistic (z0)


[h, p, ci, zval] =  ztest(X, miu0, sigma, alpha, 'left');

if h == 0,
    fprintf('Accept the test hypothesis (efficiency standard is met)\n')
else
    fprintf('Reject the test hypothesis (efficiency standard not met)\n')
end

fprintf('Observed value of the test statistic: %3.3f\n', zval);
fprintf('P-value: %3.3f\n', p);

talpha = norminv(alpha, 0, 1); % quantile
fprintf('Rejection region: (-inf, %3.3f)\n', talpha)


% b)

% H0 sigma^2  =  25
% H1 sgima^2 =/= 25

fprintf('\nb)\n')

n = length(X);
v0 = 25;
[h, p, ci, stats] = vartest(X, 2, 'alpha', alpha, 'tail', 0);

if h == 0,
    fprintf('Accept the test hypothesis (sigma = 5)\n')
else
    fprintf('Reject the test hypothesis (sigma =/= 5)\n')
end

fprintf('Observed value of the test statistic: %3.3f\n', stats.chisqstat);
fprintf('P-value: %3.3f\n', p);


qleft = chi2inv(alpha/2, n-1);    % quantile
qright = chi2inv(1-alpha/2, n-1); % quantile
fprintf('Rejection region: (-inf, %3.3f) U (%3.3f, inf)\n', qleft, qright)


%%% PROBLEM 2 %%%

pause;
clear all

fprintf('PROBLEM 2\n')
fprintf('H0 miu = 99.4 (< 99.4)\n')
fprintf('H1 miu > 99.4 (right-tailed test)\n')
fprintf('\n')


alpha = input('significance level = ');

X = [99.8 * ones(1, 2)  99.9 * ones(1, 5), 98.0 * ones(1, 3), ...
     100.1 * ones(1, 4), 100.5 * ones(1, 2), 100.0 * ones(1, 2), ...
     100.2 * ones(1, 2)];

n = length(X);

[h, p, ci, stats] = ttest(X, 2, 'alpha', alpha, 'tail', 'right');

%       => tstat (observed value of the test statistic)
% stats => df (degrees of freedon of the test)
%       => sd (estimated population standard deviation)

if h == 0,
    fprintf('Accept the test hypothesis\n')
    fprintf('The center will accept the energy bars\n')
else
    fprintf('Reject the test hypothesis\n')
    fprintf('The center will NOT accept the energy bars\n')
end

fprintf('Observed value of the test statistic: %3.3f\n', stats.tstat);
fprintf('P-value: %3.3f\n', p);

q = tinv(1-alpha, n-1); % quantile
fprintf('Rejection region: (%3.3f, inf)\n', q)


