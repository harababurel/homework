a = [1 2 3]
# a = [1;2;3]
b = [4;5;6]
d = b'


a * b
a.*b

[maximum, index] = max(a)

a_extended = [a 3 3]

[maximum_extended, index_extended] = max(a_extended)

a_concat = [a a_extended]

A = [1 2 3; 4 5 6; 7 8 9]

mean(a)

diag(a)
diag(diag(a))
diag(diag(diag(a)))
diag(diag(diag(diag(a))))

zeros(3, 2)

A \ b # solves a linear system: A * x = b (???)

A == (A')

A = magic(10); # semicolon hides the output

B = A(2:9, 3:end)

p = [1 0 0 1]; # x^3 + 1
polyval(p, 5)  # 5^3 + 1

f = @(x) x.^3 + 1 # .^ operator also works for arrays

f(5)
f([1 2 3])

roots([21 (-17) (-5) 1])

# plot([-2, 8], [5, -1])

f = @(x) exp(10*x.*(x-1)) .* sin(12*pi*x)

xs = [0:0.01:1];
plot(xs, f(xs), '-*r');



fx = @(t, a, b) (a+b)*cos(t) - b*cos((a/b + 1)*t)
fy = @(t, a, b) (a+b)*sin(t) - b*sin((a/b + 1)*t)

xs = [0:0.1:10*pi];

a = 5.5;
b = 1;
plot(fx(xs, a, b), fy(xs, a, b));

# doesn't work:
# pw = @(x) piecewise(mod(x, 2)==0, x/2, mod(x, 2)==1, 3*x+1)
# plot([0:50], pw([0:50]))

function ret = f(n)
   if n == 0
     ret = 1;
   else
     ret = 1 + 1 / (1 + f(n-1));
   end
 end

f(200)
sqrt(2)
