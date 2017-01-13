f([], 0).
f([H|T], S) :-
    f(T, S1),
    S is S1+H.
%   S1 is S-H.
%         ^ S is not instantiated at this point, so the recursion
%           can not continue, as S1 is not computed.
