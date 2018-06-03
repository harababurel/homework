#xs = [2,4,6,8]
#fs = [4,8,14,16]

xs = [0, 1, 2, 4]
fs = [3, 4, 7, 19]

first_order = arrayfun(@(i) (fs(i) - fs(i-1)) / (xs(i) - xs(i-1)),
                       2:length(xs))

second_order = arrayfun(@(i) (first_order(i) - first_order(i-1)) / (xs(i+1) - xs(i-1)),
                        2:length(first_order))


function divided_differences = f(order, xs)
  if order == 0
    divided_differences = arrayfun(@(i) (fs(i) - fs(i-1)) / (xs(i) - xs(i-1)), 2:length(xs))
  else
    divided_differences = arrayfun(@(i) (f(order-1, xs)(i) - first_order(i-1)) / (xs(i+1) - xs(i-1)),
                        2:length(first_order))
  endif
endfunction

# [xs' first_order']
