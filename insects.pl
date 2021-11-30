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
        possible_placements/3,
        possible_moves/5,
        move_insect/5
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
    findall(Hex_other_player, insect(_, _, Other_player_id, Hex_other_player, true), Hexagons),
    hexagon:anti_neighborhood(Void_neighbors, Hexagons, Placements).

% possible moves of any insect
% possible_moves(Player_id, Type, Hexagon, Moves or MSG, Staus_Code)
possible_moves(Player_id, _, _, MSG, Status_Code):-
    not(insect(queen_bee, _, Player_id, _, true)),
    MSG = "Add your QUEEN if you want to move!",
    Status_Code = 400,
    !.
possible_moves(Player_id, Type, Hexagon, MSG, Status_Code):-
    not(insect(Type, _, Player_id, Hexagon, _)),
    hexagon:get_QR(Hexagon, Q, R),
    string_concat(Player_id, " must have a ", MSG1),string_concat(MSG1, Type, MSG2),string_concat(MSG2, " unlocked in the hexagon [", MSG3),
    string_concat(MSG3, Q, MSG4),string_concat(MSG4, ",", MSG5),string_concat(MSG5, R, MSG6),string_concat(MSG6, "]", MSG),
    Status_Code = 400,
    !.
possible_moves(Player_id, Type, Hexagon, Moves, Status_Code):-
    switch(Type,
        [
            queen_bee: queen_bee_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            beetle: beetle_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            grasshopper: grasshopper_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            spider: spider_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            soldier_ant: soldier_ant_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            ladybug: ladybug_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            mosquito: mosquito_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            pillbug: pillbug_possible_moves(Player_id, Hexagon, Moves, Status_Code)
        ]).

% switch case {key: value=fun}s
switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).

% queen bee possible moves
queen_bee_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true),
    get_hive_hexagons(Hive_hex),
    hexagon:is_a_hinged_hex(Hexagon, Hive_hex),
    MSG = "Cannot move because this would break the hive in 2.",
    Status_Code = 400,
    !.
queen_bee_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            has_at_least_one_neighbor_placed_other_than_hex(X, Hexagon),
            road_blocked(X, Hexagon)
        ),
        L),
        L == [],
        MSG = 'Insect has no allowed destination.',
        Status_Code = 400.
queen_bee_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            has_at_least_one_neighbor_placed_other_than_hex(X, Hexagon),
            road_blocked(X, Hexagon)
        ),
        Moves),
    Status_Code = 200.

beetle_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(beetle, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
grasshopper_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
spider_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(spider, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
soldier_ant_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(soldier_ant, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
ladybug_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(ladybug, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
mosquito_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(mosquito, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
pillbug_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(pillbug, _, Player_id, Hexagon, true),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.

% get void neighbor of Player_id's Hex(Player_id, Hex, Void_neighbors)
get_void_neighbors_of_hex(Player_id, Hex, Void_neighbors):-
    other_player(Player_id, Other_player_id),
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H1, insect(_, _, Player_id, H1, true), Hexagons1),
    findall(H2, insect(_, _, Other_player_id, H2, true), Hexagons2),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons1)), not(member(X, Hexagons2))), Void_neighbors).

% get placed neighbor of Hex(Hex, Void_neighbors)
get_placed_neighbors_of_hex(Hex, Placed_neighbors):-
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true), Hexagons),
    findall(X, (member(X, Neighbors), member(X, Hexagons)), Placed_neighbors).

% H2 is blocked for H1 if they are adjacent and the two adjacent ones that they have in common are not empty
road_blocked(H1, H2):-
    hexagon:axial_neighbors(H1,N1),
    hexagon:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    findall(X, (member(X,Set), (insect(_,_,_,X, true))),L),
    length(L,Len),
    Len < 2.
road_blocked(H1, H2):-
    hexagon:axial_neighbors(H1,N1),
    hexagon:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    findall(X, (member(X,Set), (insect(_,_,_,X, true))),L),
    length(L,Len),
    Len == 2,
    fail,!.

    % write("Intersection: "),write(Set),nl,
    % write("L: "),write(L),nl.

% if X has at least one neighbor placed other than Hex
has_at_least_one_neighbor_placed_other_than_hex(X, _):-
    get_placed_neighbors_of_hex(X, Placed_Neighbors),
    length(Placed_Neighbors, Len),
    Len == 0,
    fail,!.
has_at_least_one_neighbor_placed_other_than_hex(X, Hex):-
    get_placed_neighbors_of_hex(X, Placed_Neighbors),
    length(Placed_Neighbors, Len),
    Len == 1,
    Placed_Neighbors == [Hex],
    fail,!.
has_at_least_one_neighbor_placed_other_than_hex(X, _):-
    get_placed_neighbors_of_hex(X, Placed_Neighbors),
    length(Placed_Neighbors, Len),
    Len > 1.


% returns all insects placed in the hive
get_hive_hexagons(Hive_hex):-
    findall(H, insect(_, _, _, H, true), Hive_hex).

% get void neighbor of all Player_id's Hex(Player_id, Void_neighbors)
get_void_neighbors_of_all_hex(Player_id, Void_neighbors):-
    findall(VN, (insect(_, _, Player_id, Hex, true), get_void_neighbors_of_hex(Player_id, Hex, VN)), Void_neighbors_aux1),
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
    insect(Type, Id, Player_id, none, false),
    !,
    retract(insect(Type, Id, Player_id, none, false)),
    assert(insect(Type, Id, Player_id, Hex, true)),
    insect(Type, Id, Player_id, Hex, true)=..Insect.

% move insect from hexagon_ori to hexagon_end
% move_insect(Player_id, Type, Hex, Insect)
move_insect(Player_id, Type, Hexagon_Ori, Hexagon_End, Insect):-
    var(Insect),
    atom(Player_id),
    atom(Type),
    compound(Hexagon_Ori),
    compound(Hexagon_End),
    var(Insect),
    insect(Type, Id, Player_id, Hexagon_Ori, true),
    !,
    retract(insect(Type, Id, Player_id, Hexagon_Ori, true)),
    assert(insect(Type, Id, Player_id, Hexagon_End, true)),
    insect(Type, Id, Player_id, Hexagon_End, true)=..Insect.


% all insect with filter
all_insects(Type, Id, Player_id, Hex, Placed, Insects):-
    findall([Type, Id, Player_id, Hex, Placed], insect(Type, Id, Player_id, Hex, Placed), Insects).
% initialize insects with default values
init_insects():-
    assert(insect(queen_bee, 1, p1, none, false)),
    assert(insect(queen_bee, 1, p2, none, false)),

    assert(insect(beetle, 1, p1, none, false)),
    assert(insect(beetle, 2, p1, none, false)),
    assert(insect(beetle, 1, p2, none, false)),
    assert(insect(beetle, 2, p2, none, false)),

    assert(insect(grasshopper, 1, p1, none, false)),
    assert(insect(grasshopper, 2, p1, none, false)),
    assert(insect(grasshopper, 3, p1, none, false)),
    assert(insect(grasshopper, 1, p2, none, false)),
    assert(insect(grasshopper, 2, p2, none, false)),
    assert(insect(grasshopper, 3, p2, none, false)),

    assert(insect(spider, 1, p1, none, false)),
    assert(insect(spider, 2, p1, none, false)),
    assert(insect(spider, 1, p2, none, false)),
    assert(insect(spider, 2, p2, none, false)),

    assert(insect(soldier_ant, 1, p1, none, false)),
    assert(insect(soldier_ant, 2, p1, none, false)),
    assert(insect(soldier_ant, 3, p1, none, false)),
    assert(insect(soldier_ant, 1, p2, none, false)),
    assert(insect(soldier_ant, 2, p2, none, false)),
    assert(insect(soldier_ant, 3, p2, none, false)),

    assert(insect(ladybug, 1, p1, none, false)),
    assert(insect(ladybug, 1, p2, none, false)),

    assert(insect(mosquito, 1, p1, none, false)),
    assert(insect(mosquito, 1, p2, none, false)),

    assert(insect(pillbug, 1, p1, none, false)),
    assert(insect(pillbug, 1, p2, none, false)).

% -------------------------------------------------------