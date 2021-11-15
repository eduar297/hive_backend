% ##################################################################################
% --------------------------------------UTILS--------------------------------------
% ##################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(utils, 
    [element_at/3,
    indexOf/3,
    concat/3,
    isList/1,
    isHex/1,
    element_hex/2,
    flatten_hex/2]).

% --------------------------------------MODULES--------------------------------------
% ...

% --------------------------------------DYNAMICS--------------------------------------
% ...

% --------------------------------------METHODS--------------------------------------
% element_at(X, L, I).
element_at(X,[X|_],1).
element_at(X,[_|L],K) :- element_at(X,L,K1), K is K1 + 1.

% indexOf(X,L,I)
indexOf(X, [X|_], 1):-!.
indexOf(X, [_|R], I):-
    indexOf(X, R, I1),
    !,
    I is I1+1.

% concat(L1, L2, L3) | L3 = L1 + L2
concat([], X, X):-!.
concat([X|R], Y, [X|Z]):-
    concat(R, Y, Z).

% isList(L)
isList([]).
isList([_|_]).

% isHex(Hex)
isHex([Q, R]):-
    number(Q),
    number(R).

% element_hex(Hex, L)
element_hex(X, [X]):-
    isHex(X),!.
element_hex(X, [X]):-
    not(isList(X)).
element_hex(X, V):-
    isList(X),
    flatten_hex(X,V).

% flatten_hex(L1, L2).
%Ex: flatten_hex([ [[1,2],[3,4]], [[5,6],[7,8]] ], [[1,2],[3,4],[5,6],[7,8]]).
flatten_hex([],[]).
flatten_hex([X|Y],R):- 
    element_hex(X,V),
    flatten_hex(Y,L),
    concat(V,L,R).