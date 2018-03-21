# Taylor

hold on

function ret = f(n)
  if n == 0
    ret = @(x) 1;
  else
    ret = @(x) f(n-1)(x) + x.^n / factorial(n);
  end
end

xs = [-1:0.01:3];
for i = 0:6
  plot(xs, f(i)(xs))
endfor
