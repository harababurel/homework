### Repeated trapezium

function result = rep_trapezium(a, b, f, pieces)
  piece_width = (b-a) / pieces;

  points = linspace(a, b, pieces+1);
  lows = points(1:end-1);
  highs = points(2:end);

  # now zip(lows, highs) contains all pieces (x, x + piece_width)
  result = sum((f(highs) + f(lows)) * piece_width) / 2;
endfunction
