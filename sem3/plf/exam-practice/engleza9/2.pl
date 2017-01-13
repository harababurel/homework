% [1, [2, 3, 4], 5, [6, 7]] -> [[7, 6], 5, [4, 3, 2], 1]

append([], X, [X]).
append([H|T], X, S) :-
    append(T, X, S1),
    S = [H|S1].

reverseList([], []).
    number(H),
    !, % do not attempt other matches (the one below) for this function
    reverseList(T, S1),
    append(S1, H, S).

reverseList([H|T], S) :-
    reverseList(H, H1),
    reverseList(T, S1),
    append(S1, H1, S).
