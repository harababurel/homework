% Nbin(n, p) generator - sum of n indep. Geo(p) variables
clear all

n = input('n: ');
p = input('p: ');

% Generate a sample
N = input('Simulations: ');
for sim = 1:N
    clear X
    for i = 1:n
        X(i) = 0; % initial value
    
        while rand >= p % while failure
            X(i) = X(i) + 1;
        end
    end
    Y(sim) = sum(X);
end


UY = unique(Y);
frq = hist(Y, length(UY));
relfrq = frq / N;

% Compare graphically with nbinpdf()
xpdf = 0:max(UY);
ypdf = nbinpdf(xpdf, n, p);

clf;
plot(xpdf, ypdf, '*', UY, relfrq, 'o');
legend('nbinpdf', 'simulation', 0);