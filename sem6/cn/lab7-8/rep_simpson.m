### Repeated trapezium

function result = rep_simpson(a, b, f, pieces)
  piece_width = (b-a) / pieces;

  points = linspace(a, b, pieces+1);
  lows = points(1:end-1);
  highs = points(2:end);

  # now zip(lows, highs) contains all pieces (x, x + piece_width)
  result = sum(piece_width / 6 * (f(lows) + 4 * f((lows+highs)/2) + f(highs)));
endfunction
