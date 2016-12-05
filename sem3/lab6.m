% Generator Bernoulli(p) - N trials
clear all
p = input('p = ');
N = input('Simulations = ');
for i=1:N
    X(i) = (rand < p);
    % fprintf('%2.f\n', X(i));
end;

UX = unique(X)

frq = hist(X, length(UX));

relfrq = frq/N