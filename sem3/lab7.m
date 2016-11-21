clf;
%a = input('a = ');
%b = input('b = ');
N = input('Sample size: ');

% x = unifrnd(a, b, 1, N); % sample

lambda = input('lambda (>0) = ');
x = exprnd(lambda, 1, N);

no_classes = 1 + 10/3 * log(N); % Sturges
limits = min(x) : (max(x) - min(x)) / no_classes : max(x);

i = 1:no_classes;
left_limits = limits(i);
right_limits = limits(i+1);

[freq, mark] = hist(x, no_classes);

rel_freq = freq / N;

results = [i; left_limits; right_limits; freq; mark; rel_freq];

fprintf(' # |      LIMITS      | FREQ |  MARK | REL. FREQ\n')
fprintf('%2d | [%5.3f, %5.3f] | %4d | %5.3f | %5.3f\n', results)

hist(x, no_classes)
hold on
plot(mark, freq, 'r')

id_mode = find(freq == max(freq));
fprintf('Mode:\n')

results_mode = [id_mode; left_limits(id_mode); right_limits(id_mode); freq(id_mode); mark(id_mode); rel_freq(id_mode)];
fprintf('%2d | [%5.3f, %5.3f] | %4d | %5.3f | %5.3f\n', results_mode)

fprintf('Mean: %5.3f\n', mean(x))


fprintf('Quartiles: %5.3f %5.3f %5.3f\n', prctile(x ,[25, 50, 75]))


