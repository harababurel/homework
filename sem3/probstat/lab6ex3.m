% Geometric(p) generator - no. of failures before first success
clear all
p = input('p: ');

% Generate a sample
N = input('Simulations: ');
for i = 1:N
    X(i) = 0; % initial value
    while rand >= p % while failure
        X(i) = X(i) + 1;
    end
end

UX = unique(X);
frq = hist(X, length(UX));
relfrq = frq / N;

% Compare graphically with geopdf()
xpdf = 0:max(UX);
ypdf = geopdf(xpdf, p);

clf;
plot(xpdf, ypdf, '*', UX, relfrq, 'o');
legend('geopdf', 'simulation', 0);