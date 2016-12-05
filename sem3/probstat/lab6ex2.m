% Binomial(n, p) generator
clear all
n = input('n: ');
p = input('p: ');

%U = rand(n, 1);
%X = sum(U < p);

% Generate a sample
N = input('Simulations: ');
for i = 1:N
    U = rand(n, 1);
    X(i) = sum(U < p);
end

UX = unique(X);
frq = hist(X, length(UX));
relfrq = frq / N;

xpdf = 0:n;
ypdf = binopdf(xpdf, n, p);

plot(xpdf, ypdf, '*', UX, relfrq, 'o');
legend('binopdf', 'simulation', 0);