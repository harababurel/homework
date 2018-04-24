### Repeated rectangle


function result = rep_rectangle(a, b, f, pieces)
  piece_width = (b-a) / pieces;

  points = linspace(a, b, pieces+1);
  lows = points(1:end-1);
  highs = points(2:end);

  result = sum(piece_width * f((lows + highs) / 2));
endfunction
