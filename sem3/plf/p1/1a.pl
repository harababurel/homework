gcd(X, 0, X).

gcd(X, Y, Z) :-
    Mod is mod(X, Y),
    gcd(Y, Mod, Z).
