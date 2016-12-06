% 15. For a given n, positive, determine all decomposition of n as a sum of consecutive natural numbers.

% Generate all values in [1..N].
getValue(N, N).
getValue(N, X) :-
    N > 1,
    Y is N-1,
    getValue(Y, X).

% Create the sequence [X..Y].
createSequence(X, Y, []) :- X > Y.
createSequence(X, X, [X]).
createSequence(X, Y, [X|Xs]):-
    X < Y,
    NewX is X+1,
    createSequence(NewX, Y, Xs).

% Check whether the sum X + (X+1) + ... + Y == N
checkSum(N, X, Y) :-
    S is Y*(Y+1)/2 - X*(X-1)/2,
    N = S.

% Generate each list of consecutive values that add up to N
generateSol(N, Xs) :-
    getValue(N, Y),            % Y <- [1..N]
    getValue(Y, X),            % X <- [1..Y]
    checkSum(N, X, Y),         % sum  [X..Y] == N
    createSequence(X, Y, Xs).  % Xs = [X..Y]


solve(N, Xss) :-
    findall(Xs, generateSol(N, Xs), Xss).
