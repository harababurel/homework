%contains([], _, 0).
contains([H|_], X) :- X is H, !.
contains([_|T], X) :-
    contains(T, X).

listToSet([], []).
listToSet([H|T], S) :-
    contains(T, H),  % the head is already present someplace else
    !,
    listToSet(T, S).

listToSet([H|T], S) :-
    listToSet(T, S1),
    S = [H|S1].

