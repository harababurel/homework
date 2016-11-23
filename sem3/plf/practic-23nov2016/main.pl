% PROLOG 39
% GRADE: 7.5

max([X], X).
max([H|T], Sol) :-
    max(T, Y),
    Y >= H,
    Sol is Y.

max([H|T], Sol) :-
    max(T, Y),
    Y < H,
    Sol is H.

% substitute(initial list, source, target, accumulator)
substitute([], _, _, []).
substitute([H|T], H, Y, [Y|Acc]) :-
    substitute(T, H, Y, Acc).

substitute([H|T], X, Y, [H|Acc]) :-
    \=(X, H),
    substitute(T, X, Y, Acc).


solve(Xs, Ys, Sol) :-
    max(Xs, M),
    substitute(Xs, M, Ys, Sol).


%=============================================
linearize([], []).

%linearize([[X|_]|Xss], [X|Acc]) :-
%    linearize(Xss, Acc).

linearize([[X|Xs]|Xss], [X|Acc]) :-
    %    \=(Xs, []),
    linearize([Xs|Xss], Acc).

linearize([X|Xs], [X|Acc]) :-
    linearize(Xs, Acc).
