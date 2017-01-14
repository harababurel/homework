% idea: http://stackoverflow.com/questions/4912869/subsets-in-prolog

% subset(X, Y) <-> Y is a subset of X
subset([], []).
subset([E|Tail], [E|NTail]) :-
    subset(Tail, NTail).

% ^if two lists have the same head,
% and the tail of the second list is a subset of the tail of the first list,
% then the second list is a subset of the first list.

subset([_|Tail], NTail):-
    subset(Tail, NTail).

% ^if the heads are not necessarily equal,
% than the second list is a subset of the first list
% only if the second tail is a subset of the first tail.

subsetN(X, N, S) :-
    subset(X, S),
    length(S, N).

% S must be a subset of X
% and length(S) must be N
