h = 0.25;
base_a = 1;

a = @(i) base_a + i*h;
f = @(x) sqrt(5*x.^2 + 1);

I = [0:6]';
A = arrayfun(a, I);
B = arrayfun(f, A);

T = zeros(length(A));
T(:,1) = B;

for j = 2:size(T)(2)
  for i = 1:size(T)(1)-j+1
    T(i, j) = T(i+1, j-1) - T(i, j-1);
  endfor
endfor

[I, T]
