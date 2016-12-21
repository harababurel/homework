% Generate range [Low, High]
range(Low, Low, _).
range(X, Low, High) :-
    NewLow is Low+1,
    NewLow =< High,
    range(X, NewLow, High).
