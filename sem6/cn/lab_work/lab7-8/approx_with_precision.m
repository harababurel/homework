function result = approx_with_precision(f, a, b, precision, method)
  for n = 2:1e8
    prev_approx = method(a, b, f, n-1);
    approx = method(a, b, f, n);
    error = abs(approx - prev_approx);

    if error <= precision
      result = approx;
      return;
    endif
  endfor
endfunction
