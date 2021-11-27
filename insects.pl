% ##################################################################################
% --------------------------------------INSECT--------------------------------------
% ##################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(insects, 
    [
        insect/5,
        place_insect/4,
        all_insects/6,
        init_insects/0,
        possible_placements/3
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(hexagon), import(hexagon).

% --------------------------------------DYNAMICS--------------------------------------
:-dynamic insect/5.
% insect(Type, Id, PlayerId, Hex=[Q,R], Placed)

% other player id
other_player(p1, p2).
other_player(p2 ,p1).

% --------------------------------------METHODS--------------------------------------
% possibles placements
% possible_placements(Player_id, Number_of_moves, Placements)
possible_placements(Player_id, Number_of_moves, Placements):-
    % the first player's first move will always play on hexagon(0,0)
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    Player_id == p1,
    Number_of_moves == 0,
    Placements = [[0, 0]],
    !.
possible_placements(Player_id, Number_of_moves, Placements):-
    % the second player's first move will always play on an neighbor hexagon of hexagon(0,0)
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    Player_id == p2,
    Number_of_moves == 0,
    hexagon:axial_neighbors([0, 0], Placements),
    !.
possible_placements(Player_id, Number_of_moves, Placements):-
    % the possible hexagons are the empty neighbors of those of Player_id
    % and that are not neighbors of the other player
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    other_player(Player_id, Other_player_id),
    get_void_neighbors_of_all_hex(Player_id, Void_neighbors),
    findall(Hex_other_player, insect(_, _, Other_player_id, Hex_other_player, yes), Hexagons),
    hexagon:anti_neighborhood(Void_neighbors, Hexagons, Placements).

% get void neighbor of Player_id's Hex(Player_id, Hex, Void_neighbors)
get_void_neighbors_of_hex(Player_id, Hex, Void_neighbors):-
    other_player(Player_id, Other_player_id),
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H1, insect(_, _, Player_id, H1, yes), Hexagons1),
    findall(H2, insect(_, _, Other_player_id, H2, yes), Hexagons2),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons1)), not(member(X, Hexagons2))), Void_neighbors).

% get void neighbor of all Player_id's Hex(Player_id, Void_neighbors)
get_void_neighbors_of_all_hex(Player_id, Void_neighbors):-
    findall(VN, (insect(_, _, Player_id, Hex, yes), get_void_neighbors_of_hex(Player_id, Hex, VN)), Void_neighbors_aux1),
    utils:flatten_hex(Void_neighbors_aux1, Void_neighbors_aux2),
    setof(X, member(X, Void_neighbors_aux2), Void_neighbors).


% place insect in Hex
% place_insect(Player_id, Type, Hex, Insect)
place_insect(Player_id, Type, Hex, Insect):-
    var(Insect),
    atom(Player_id),
    atom(Type),
    compound(Hex),
    var(Insect),
    insect(Type, Id, Player_id, none, no),
    !,
    retract(insect(Type, Id, Player_id, none, no)),
    assert(insect(Type, Id, Player_id, Hex, yes)),
    insect(Type, Id, Player_id, Hex, yes)=..Insect.

% all insect with filter
all_insects(Type, Id, Player_id, Hex, Placed, Insects):-
    findall([Type, Id, Player_id, Hex, Placed], insect(Type, Id, Player_id, Hex, Placed), Insects).
% initialize insects with default values
init_insects():-
    assert(insect(queen_bee, 1, p1, none, no)),
    assert(insect(queen_bee, 1, p2, none, no)),

    assert(insect(beetle, 1, p1, none, no)),
    assert(insect(beetle, 2, p1, none, no)),
    assert(insect(beetle, 1, p2, none, no)),
    assert(insect(beetle, 2, p2, none, no)),

    assert(insect(grasshopper, 1, p1, none, no)),
    assert(insect(grasshopper, 2, p1, none, no)),
    assert(insect(grasshopper, 3, p1, none, no)),
    assert(insect(grasshopper, 1, p2, none, no)),
    assert(insect(grasshopper, 2, p2, none, no)),
    assert(insect(grasshopper, 3, p2, none, no)),

    assert(insect(spider, 1, p1, none, no)),
    assert(insect(spider, 2, p1, none, no)),
    assert(insect(spider, 1, p2, none, no)),
    assert(insect(spider, 2, p2, none, no)),

    assert(insect(soldier_ant, 1, p1, none, no)),
    assert(insect(soldier_ant, 2, p1, none, no)),
    assert(insect(soldier_ant, 3, p1, none, no)),
    assert(insect(soldier_ant, 1, p2, none, no)),
    assert(insect(soldier_ant, 2, p2, none, no)),
    assert(insect(soldier_ant, 3, p2, none, no)),

    assert(insect(ladybug, 1, p1, none, no)),
    assert(insect(ladybug, 1, p2, none, no)),

    assert(insect(mosquito, 1, p1, none, no)),
    assert(insect(mosquito, 1, p2, none, no)),

    assert(insect(pillbug, 1, p1, none, no)),
    assert(insect(pillbug, 1, p2, none, no)).