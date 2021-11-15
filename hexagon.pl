% ##################################################################################
% --------------------------------------HEXAGON--------------------------------------
% ##################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(hexagon,
    [
        cardinal_direction_to_axial/3,
        axial_neighbors/2,
        are_neighbors/2,
        anti_neighborhood/3
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(utils), import(utils).

% --------------------------------------DYNAMICS--------------------------------------
% ...

% --------------------------------------Cardinal Direction--------------------------------------
% cardinal_direction_to_axial(NumericDir, CardinalDir, Hex).
cardinal_direction_to_axial(1, n, [0, -1]).
cardinal_direction_to_axial(2, nw, [-1, 0]).
cardinal_direction_to_axial(3, sw, [-1, 1]).
cardinal_direction_to_axial(4, s, [0, 1]).
cardinal_direction_to_axial(5, se, [1, 0]).
cardinal_direction_to_axial(6, ne, [1, -1]).

% --------------------------------------METHODS--------------------------------------

% --------------------------------------Coordinate conversion--------------------------------------
% cube_to_axial(Cube, Hex)
cube_to_axial([Q, R, _], Hex):-
    number(Q),
    number(R),
    var(Hex),
    Hex = [Q, R].

% axial_to_cube(Hex, Cube)
axial_to_cube([Q, R], Cube):-
    number(Q),
    number(R),
    var(Cube),
    S is -Q-R,
    Cube = [Q, R, S].

% axial_to_oddr(Hex, OffsetCoord)
axial_to_oddr([Q, R], OffsetCoord):-
    number(Q),
    number(R),
    var(OffsetCoord),
    Col is Q + (R - (R mod 2)) / 2,
    Row = R,
    OffsetCoord = [Col, Row].

% oddr_to_axial(OffsetCoord, Hex)
oddr_to_axial([Col, Row], Hex):-
    number(Col),
    number(Row),
    var(Hex),
    Q = Col - (Row - (Row mod 2)) / 2,
    R = Row,
    Hex = [Q, R].

% --------------------------------------Neighbors--------------------------------------
% axial_direction_vectors(Adv)
axial_direction_vectors(Axial_direction_vectors):-
    findall(Hex, cardinal_direction_to_axial(_, _, Hex), Axial_direction_vectors).

% axial_direction(Dir, Axial_direction_vector)
axial_direction(Dir, Vec):-
    axial_direction_vectors(Axial_direction_vectors),
    utils:element_at(Vec, Axial_direction_vectors, Dir).

% axial_add(Hex1, Vec, Hex2)
axial_add([Q1, R1], [Q2, R2], Hex):-
    Q3 is Q1 + Q2,
    R3 is R1 + R2,
    Hex = [Q3, R3].

% axial_neighbor(Hex, Dir, Neighbor)
axial_neighbor(Hex, Dir, Neighbor):-
    axial_direction(Dir, Vec),
    axial_add(Hex, Vec, Neighbor).

% axial_neighbors(Hex, Neighbors)
axial_neighbors(Hex, Neighbors):-
    findall(Neighbor, axial_neighbor(Hex, _, Neighbor), Neighbors).

% true if Hex1 and Hex2 are neighbors.
are_neighbors(Hex1, Hex2):-
    axial_neighbors(Hex1, N),
    member(Hex2, N),!.

% elements of the first list that are not neighbors of any element of the second list
% anti_neighborhood(L1, L2, L3)
anti_neighborhood(L1, L2, L3):-
    findall(X, (member(X, L1),not(be_a_neighbor(X, L2))), L3).

be_a_neighbor(Hex, Hexs):-
    member(H, Hexs),
    are_neighbors(Hex, H),!.




% anti_neighborhood([[-1,1],[-1,3],[0,3],[1,0],[1,1],[1,2]], [[0,0],[-1,2]], X).