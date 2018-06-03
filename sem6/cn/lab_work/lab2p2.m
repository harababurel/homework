# Chebyshev

chebyshev = @(n, t) cos(n * acos(t));

xs = [-1:0.01:1];
hold on
# plot(xs, chebyshev(1, xs));
# plot(xs, chebyshev(2, xs));
# plot(xs, chebyshev(3, xs));

function ret = f(n)
  if n == 0
    ret = @(x) 1;
  elseif n == 1
    ret = @(x) x;
  else
    ret = @(x) 2.*x.*f(n-1)(x) - f(n-2)(x);
  end
end

for i = 1:10
  plot(xs, f(i)(xs))
endfor

title("Chebyshev_n(x) for x in [-1..1], n=1..10")
