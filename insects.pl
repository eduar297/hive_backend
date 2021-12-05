% ##################################################################################
% --------------------------------------INSECT--------------------------------------
% ##################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(insects, 
    [
        insect/6,
        place_insect/4,
        all_insects/7,
        init_insects/0,
        possible_placements/3,
        possible_moves/6,
        move_insect/7
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(hexagon), import(hexagon).

% --------------------------------------DYNAMICS--------------------------------------
:-dynamic insect/6.
% insect(Type, Id, PlayerId, Hex=[Q,R], Placed, Lvl)

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
    findall(Hex_other_player, insect(_, _, Other_player_id, Hex_other_player, true, _), Hexagons),
    hexagon:anti_neighborhood(Void_neighbors, Hexagons, Placements).

% possible moves of any insect
% possible_moves(Player_id, Type, Hexagon, Moves or MSG, Staus_Code)
possible_moves(Player_id, _, _, MSG, Status_Code):-
    not(insect(queen_bee, _, Player_id, _, true,_)),
    MSG = "Add your QUEEN if you want to move!",
    Status_Code = 400,
    !.
possible_moves(Player_id, Type, Id, Hexagon, MSG, Status_Code):-
    insect(Type, Id, Player_id, Hexagon, true, Lvl),
    is_blocked(Type,Id,Player_id,Hexagon),
    % tell('./log'),
    % write([Type,Id,Player_id,Hexagon,Lvl]),
    % told,
    MSG = "Insect blocked!",
    Status_Code = 400,
    !.
possible_moves(Player_id, Type, Id, Hexagon, MSG, Status_Code):-
    insect(Type, Id, Player_id, Hexagon, true, _),
    get_hive_hexagons(Hive_hex),
    hexagon:is_a_hinged_hex(Hexagon, Hive_hex),
    MSG = "Cannot move because this would break the hive in 2.",
    Status_Code = 400,
    !.
possible_moves(Player_id, Type, Id, Hexagon, Moves, Status_Code):-
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

% switch case {key: value=fun}
switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).

% get_possible_move_in_direction(Dir, Hex)
get_possible_move_in_direction(_, Hex, PM, N):-
    N == 0,
    is_an_empty_hex(Hex),
    PM = [],!.
get_possible_move_in_direction(Dir, Hex, PM, N):-
    N == 0,
    not(is_an_empty_hex(Hex)),
    hexagon:axial_neighbor(Hex, Dir, NHex),
    N1 is N+1,
    get_possible_move_in_direction(Dir, NHex, PM, N1).

get_possible_move_in_direction(_, Hex, PM, N):-
    N > 0,
    is_an_empty_hex(Hex),
    PM = Hex,!.
get_possible_move_in_direction(Dir, Hex, PM, N):-
    N > 0,
    not(is_an_empty_hex(Hex)),
    hexagon:axial_neighbor(Hex, Dir, NHex),
    N1 is N+1,
    get_possible_move_in_direction(Dir, NHex, PM, N1).

% queen bee possible moves
queen_bee_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true, 0),
    get_void_neighbors_of_hex_2( Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            has_at_least_one_neighbor_placed_other_than_hex(X, Hexagon),
            road_blocked(X, Hexagon)
        ),L),
        L == [],
        MSG = 'Queen bee has no allowed destination.',
        Status_Code = 400,!.
queen_bee_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true, 0),
    get_void_neighbors_of_hex_2(Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            has_at_least_one_neighbor_placed_other_than_hex(X, Hexagon),
            road_blocked(X, Hexagon)
        ),Moves),
    Status_Code = 200.

% beetle possible moves
beetle_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    get_last_insect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl == 0,
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            has_at_least_one_neighbor_placed_other_than_hex(X, Hexagon),
            road_blocked(X, Hexagon)
        ),L1),
    get_placed_neighbors_of_hex(Hexagon, L2),
    append(L1, L2, Moves),
    Moves == [],
    MSG = 'Beetle has no allowed destination.',
    Status_Code = 400,!.
beetle_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    get_last_insect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl > 0,
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    get_placed_neighbors_of_hex(Hexagon, PN),
    append(VN, PN, Moves),
    Moves == [],
    MSG = 'Beetle has no allowed destination.',
    Status_Code = 400,!.
beetle_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    get_last_insect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl == 0,
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            has_at_least_one_neighbor_placed_other_than_hex(X, Hexagon),
            road_blocked(X, Hexagon)
        ),L1),
    get_placed_neighbors_of_hex(Hexagon, L2),
    append(L1, L2, Moves),
    Status_Code = 200,!.
beetle_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    get_last_insect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl > 0,
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    get_placed_neighbors_of_hex(Hexagon, PN),
    append(VN, PN, Moves),
    Status_Code = 200,!.

% grasshopper possible moves
grasshopper_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagon, true, 0),
    hexagon:axial_neighbor(Hexagon, 1, NW),
    hexagon:axial_neighbor(Hexagon, 2, W),
    hexagon:axial_neighbor(Hexagon, 3, SW),
    hexagon:axial_neighbor(Hexagon, 4, SE),
    hexagon:axial_neighbor(Hexagon, 5, E),
    hexagon:axial_neighbor(Hexagon, 6, NE),
    
    get_possible_move_in_direction(1, NW, PM1, 0),
    get_possible_move_in_direction(2, W, PM2, 0),
    get_possible_move_in_direction(3, SW, PM3, 0),
    get_possible_move_in_direction(4, SE, PM4, 0),
    get_possible_move_in_direction(5, E, PM5, 0),
    get_possible_move_in_direction(6, NE, PM6, 0),
    
    union([PM1,PM2,PM3,PM4,PM5,PM6] ,[], L),
    utils:delete_all_occurrences([], L ,L1),
    L1 == [],
    MSG = 'Grasshopper bee has no allowed destination.',
    Status_Code = 400,!.
grasshopper_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagon, true, 0),
    hexagon:axial_neighbor(Hexagon, 1, NW),
    hexagon:axial_neighbor(Hexagon, 2, W),
    hexagon:axial_neighbor(Hexagon, 3, SW),
    hexagon:axial_neighbor(Hexagon, 4, SE),
    hexagon:axial_neighbor(Hexagon, 5, E),
    hexagon:axial_neighbor(Hexagon, 6, NE),
    
    get_possible_move_in_direction(1, NW, PM1, 0),
    get_possible_move_in_direction(2, W, PM2, 0),
    get_possible_move_in_direction(3, SW, PM3, 0),
    get_possible_move_in_direction(4, SE, PM4, 0),
    get_possible_move_in_direction(5, E, PM5, 0),
    get_possible_move_in_direction(6, NE, PM6, 0),
    
    union([PM1,PM2,PM3,PM4,PM5,PM6],[], L),
    utils:delete_all_occurrences([],L ,Moves),
    Status_Code = 200.

% spider possible moves
spider_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(spider, _, Player_id, Hexagon, true, _),
    bfs_ant([[Hexagon, 0]], []),!,
    findall(U, (node(U, Lvl), Lvl == 3), Moves0),
    retractall(node(_, _)),
    get_void_neighbors_of_hex_2(Hexagon, VN),
    findall(X, (member(X, VN), not(has_at_least_one_neighbor_placed_other_than_hex(X, Hexagon))), L),
    utils:delete2([Hexagon|L], Moves0, Moves),
    Status_Code = 200.

% soldier ant possible moves
soldier_ant_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(soldier_ant, _, Player_id, Hexagon, true, _),
    bfs_ant([[Hexagon, 0]], []),!,
    findall(U, node(U, _), Moves0),
    retractall(node(_, _)),
    get_void_neighbors_of_hex_2(Hexagon, VN),
    findall(X, (member(X, VN), not(has_at_least_one_neighbor_placed_other_than_hex(X, Hexagon))), L),
    utils:delete2([Hexagon|L], Moves0, Moves),
    Status_Code = 200.

:-dynamic node/2.

% bfs (Queue, Lvl)
bfs_ant([], _):-!.
bfs_ant([[U, _]|Q], Visited):-
    member(U, Visited),
    bfs_ant(Q, Visited).

bfs_ant([[U, Lvl]|Q], Visited):-
    not(member(U, Visited)),
    assert(node(U, Lvl)),
    valid_adj([U, Lvl], A),
    append([U], Visited, Visited1),
    append(Q, A, Q1),
    bfs_ant(Q1, Visited1).

% get all valid adj of the ant
valid_adj([U, Lvl], A):-
    Lvl1 is Lvl + 1,
    get_void_neighbors_of_hex_2(U, N),

    findall([V, Lvl1], 
        (
            member(V,N),
            has_at_least_one_neighbor_placed(V),
            road_blocked(U, V)
        ), A).



ladybug_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(ladybug, _, Player_id, Hexagon, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
mosquito_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(mosquito, _, Player_id, Hexagon, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
pillbug_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(pillbug, _, Player_id, Hexagon, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.

% get void neighbor of Player_id's Hex(Player_id, Hex, Void_neighbors)
get_void_neighbors_of_hex(Player_id, Hex, Void_neighbors):-
    other_player(Player_id, Other_player_id),
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H1, insect(_, _, Player_id, H1, true,_), Hexagons1),
    findall(H2, insect(_, _, Other_player_id, H2, true,_), Hexagons2),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons1)), not(member(X, Hexagons2))), Void_neighbors).

% get void neighbor of Hex(Hex, Void_neighbors)
get_void_neighbors_of_hex_2(Hex, Void_neighbors):-
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true,_), Hexagons),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons))), Void_neighbors).

% get placed neighbor of Hex(Hex, Void_neighbors)queen bee
get_placed_neighbors_of_hex(Hex, Placed_neighbors):-
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true, _), Hexagons),
    findall(X, (member(X, Neighbors), member(X, Hexagons)), Placed_neighbors).

% H2 is blocked for H1 if they are adjacent and the two adjacent ones that they have in common are not empty
road_blocked(H1, H2):-
    hexagon:axial_neighbors(H1,N1),
    hexagon:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    setof(X, (member(X,Set), (insect(_,_,_,X, true,_))),L),
    length(L,Len),
    Len < 2.
road_blocked(H1, H2):-
    hexagon:axial_neighbors(H1,N1),
    hexagon:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    setof(X, (member(X,Set), (insect(_,_,_,X, true,_))),L),
    length(L,Len),
    Len == 2,
    fail,!.

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
% -------------------------------------------------------------
has_at_least_one_neighbor_placed(U):-
    get_placed_neighbors_of_hex(U, PN),
    length(PN, Len),
    Len > 0.


% returns all insects placed in the hive
get_hive_hexagons(Hive_hex):-
    findall(H, insect(_, _, _, H, true,_), Hive_hex).

% get void neighbor of all Player_id's Hex(Player_id, Void_neighbors)
get_void_neighbors_of_all_hex(Player_id, Void_neighbors):-
    findall(VN, (insect(_, _, Player_id, Hex, true,_), get_void_neighbors_of_hex(Player_id, Hex, VN)), Void_neighbors_aux1),
    utils:flatten_hex(Void_neighbors_aux1, Void_neighbors_aux2),
    setof(X, member(X, Void_neighbors_aux2), Void_neighbors).
% get void neighbor of all Hex(Void_neighbors)
get_void_neighbors_of_all_hex_2(Void_neighbors):-
    findall(VN, (insect(_, _, _, Hex, true,_), get_void_neighbors_of_hex_2(Hex, VN)), Void_neighbors_aux),
    utils:flatten_hex(Void_neighbors_aux, Void_neighbors_aux1),
    setof(X, member(X, Void_neighbors_aux1), Void_neighbors).

% place insect in Hex
% place_insect(Player_id, Type, Hex, Insect)
place_insect(Player_id, Type, Hex, Insect):-
    var(Insect),
    atom(Player_id),
    atom(Type),
    compound(Hex),
    var(Insect),
    insect(Type, Id, Player_id, none, false,-1),
    !,
    retract(insect(Type, Id, Player_id, none, false,-1)),
    assert(insect(Type, Id, Player_id, Hex, true,0)),
    insect(Type, Id, Player_id, Hex, true,0)=..Insect.

% move insect from hexagon_ori to hexagon_end
% move_insect(Player_id, Type, Hex, Insect)
move_insect(Type, Id, Player_id, Lvl, Hexagon_Ori, Hexagon_End, InsectRes):-
    retract(insect(Type, Id, Player_id, Hexagon_Ori, true, Lvl)),

    get_max_lvl_in_hex(Hexagon_End, Lvl_end),

    Lvl1 is Lvl_end + 1,
    tell('./log'),
    write(Lvl_end),
    told,

    assert(insect(Type, Id, Player_id, Hexagon_End, true, Lvl1)),
    InsectRes = [Type, Id, Player_id, Hexagon_End, true, Lvl1],!.


% all insect with filter
all_insects(Type, Id, Player_id, Hex, Placed, Lvl, Insects):-
    findall([Type, Id, Player_id, Hex, Placed, Lvl], insect(Type, Id, Player_id, Hex, Placed, Lvl), Insects).
% initialize insects with default values
init_insects():-
    assert(insect(queen_bee, 1, p1, none, false, -1)),
    assert(insect(queen_bee, 1, p2, none, false, -1)),

    assert(insect(beetle, 1, p1, none, false, -1)),
    assert(insect(beetle, 2, p1, none, false, -1)),
    assert(insect(beetle, 1, p2, none, false, -1)),
    assert(insect(beetle, 2, p2, none, false, -1)),

    assert(insect(grasshopper, 1, p1, none, false, -1)),
    assert(insect(grasshopper, 2, p1, none, false, -1)),
    assert(insect(grasshopper, 3, p1, none, false, -1)),
    assert(insect(grasshopper, 1, p2, none, false, -1)),
    assert(insect(grasshopper, 2, p2, none, false, -1)),
    assert(insect(grasshopper, 3, p2, none, false, -1)),

    assert(insect(spider, 1, p1, none, false, -1)),
    assert(insect(spider, 2, p1, none, false, -1)),
    assert(insect(spider, 1, p2, none, false, -1)),
    assert(insect(spider, 2, p2, none, false, -1)),

    assert(insect(soldier_ant, 1, p1, none, false, -1)),
    assert(insect(soldier_ant, 2, p1, none, false, -1)),
    assert(insect(soldier_ant, 3, p1, none, false, -1)),
    assert(insect(soldier_ant, 1, p2, none, false, -1)),
    assert(insect(soldier_ant, 2, p2, none, false, -1)),
    assert(insect(soldier_ant, 3, p2, none, false, -1)),

    assert(insect(ladybug, 1, p1, none, false, -1)),
    assert(insect(ladybug, 1, p2, none, false, -1)),

    assert(insect(mosquito, 1, p1, none, false, -1)),
    assert(insect(mosquito, 1, p2, none, false, -1)),

    assert(insect(pillbug, 1, p1, none, false, -1)),
    assert(insect(pillbug, 1, p2, none, false)).

% -------------------------------------------------------
% if Hex is empty
is_an_empty_hex(Hex):-
    not(insect(_, _, _, Hex, true,_)),!.

% an insect is blocked if there is another insect with a hingher level in the same hexagon
is_blocked(Type,Id,Pid,Hex):-
    insect(Type, Id, Pid, Hex, true, Lvl),
    Lvl1 is Lvl+1,
    insect(_, _, _, Hex, true, Lvl1),!.

% return max lvl in Hex
get_max_lvl_in_hex(Hex, Lvl):-
    is_an_empty_hex(Hex),
    Lvl is -1,!.
get_max_lvl_in_hex(Hex, Lvl):-
    findall(Lvl1, insect(_,_,_,Hex,true,Lvl1), L),
    utils:last_element(Lvl,L).

% get the insect with the max lvl in Hex
get_last_insect(Hex, I):-
    is_an_empty_hex(Hex),
    I=[],!.
get_last_insect(Hex, I):-
    get_max_lvl_in_hex(Hex, Lvl),
    insect(Type, Id, Pid, Hex, true, Lvl),
    I = [Type, Id, Pid, Hex, true, Lvl].