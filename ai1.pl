
% ##################################################################################
% ------------------------ARTIFICIAL INTELLIGENCE | MONTE CARLO TREE SEARCH----------------------
% ##################################################################################

% % --------------------------------------EXPORTS--------------------------------------
:- module(ai1, 
    [
        play_greedy/6
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(insects), import(insects).
:-consult(utils), import(utils).
:-consult(hexagon), import(hexagon).

% --------------------------------------DYNAMICS--------------------------------------
% ...

% --------------------------------------METHODS--------------------------------------

play_greedy(Player_id, Name, Number_of_moves, Queen_bee_placed, Status_Code, MSG):-
    insects:other_player(Player_id, Other_player_id),
    P1 = Player_id,
    P2 = Other_player_id,


    insects:all_insects(_, _, P1, _, false, _, Non_placed_insects_p1),
    insects:all_insects(_, _, P2, _, false, _, Non_placed_insects_p2),
    
    insects:all_insects(_, _, P1, _, true, _, Placed_insects_p1),
    insects:all_insects(_, _, P2, _, true, _, Placed_insects_p2),

    analize_type_of_play(P1, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code).

analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves == 0,
    insects:place_insect(Player_id, beetle, [1, 0], _),
    string_concat(Name, " places ", S1),
    string_concat(S1, beetle, MSG),
    Status_Code = 200,
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves == 1,
    L = [[2,-1],[1,1]],
    random_between(1, 2, Rd),
    utils:element_at(Hex, L, Rd),
    insects:place_insect(Player_id, queen_bee, Hex, _),
    string_concat(Name, " places ", S1),
    string_concat(S1, queen_bee, MSG),
    Status_Code = 200,
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves == 2,
    insects:is_an_empty_hex([2,0]),
    insects:place_insect(Player_id, soldier_ant, [2, 0], _),
    string_concat(Name, " places ", S1),
    string_concat(S1, soldier_ant, MSG),
    Status_Code = 200,
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves == 2,
    insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Queen_bee_placed, InsectsPlacements, Placements),
    length(Placements, Len_placements),
    random_between(1, Len_placements, Rd_placements),
    utils:element_at(Hex, Placements, Rd_placements),
    insects:place_insect(Player_id, soldier_ant, Hex, _),
    string_concat(Name, " places ", S1),
    string_concat(S1, soldier_ant, MSG),
    Status_Code = 200,
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:other_player(Player_id, Other_player_id),
    insects:insect(queen_bee, 1, Other_player_id, Hex, true, 0),
    insects:amount_neighbors(Hex, A),
    A == 5,

    insects:get_void_neighbors_of_hex_2(Hex, [VN|_]),

    insects:can_move_any_of_the_insects(Player_id, InsectMoves),
    
    findall((H:VN), (member((H:M), InsectMoves), member(VN, M)), L),
    
    findall(H1, (member((H1:_), L), not(hexagon:are_neighbors(H1, Hex))), [L1|_]),

    insects:get_last_insect(L1, [Type,Id,Pid,_,true,Lvl]),

    insects:move_insect(Type, Id, Pid, Lvl, L1, VN, _),

    Status_Code = 200,
    string_concat(Name, " moves ", S1),
    string_concat(S1, Type, MSG),
    !.

analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, Moves, SC),
    SC == 200,
    insects:amount_neighbors(Hex1, A1),
    
    insects:other_player(Player_id, Other_player_id),
    insects:insect(queen_bee, Id2, Other_player_id, Hex2, true, Lvl2),
    insects:amount_neighbors(Hex2, A2),
    
    A2 < 4,
    A1 > 2,

    best_move([queen_bee, Id1, Player_id, Hex1, true, Lvl1], M),
    
    (V:H)=M,

    insects:move_insect(queen_bee, Id1, Player_id, Lvl1, Hex1, H, _),

    Status_Code = 200,
    string_concat(Name, " moves ", S1),
    string_concat(S1, queen_bee, MSG),
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 200,
    
    findall(Hex, (insect(Type, _, Player_id, Hex, true, _), Type \= queen_bee), Hexagons),
    
    findall(([I,V,M]), (member(PN, Hexagons), insects:get_last_insect(PN, I), best_move(I, M), [_,_,_,H,_,_]=I ,calculate_hexagon_total_value(H, V)), Moves),
    
    findall([I1,V2,H2], (member([I1,V1,(V2:H2)], Moves), V2 > V1 ), L),

    L \= [],
    
    max_move_value2(L, Max),
    
    [[Type, Id, _, Hex_Ori, true, Lvl],_,Hex_End] = Max,
    
    insects:move_insect(Type, Id, Player_id, Lvl, Hex_Ori, Hex_End, _),

    Status_Code = 200,
    string_concat(Name, " moves ", S1),
    string_concat(S1, Type, MSG),
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 200,

    insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Queen_bee_placed, InsectsPlacements, Placements),
    
    findall((Score1:Type), (member([Type, Id, Player_id, none, false, -1], InsectsPlacements), insects:score(Type, Player_id, Score), Score1 is -1*Score),L),

    max_move_value(L, (S:T)),
    
    length(Placements, Len_placements),
    random_between(1, Len_placements, Rd_placements),
    
    % length(InsectsPlacements, Len_insects_placements),
    % random_between(1, Len_insects_placements, Rd_insects_placements),
    
    element_at(Hex, Placements, Rd_placements),
    % element_at([Type|_], InsectsPlacements, Rd_insects_placements),
    
    insects:place_insect(Player_id, T, Hex, _),

    Status_Code=200,
    string_concat(Name, " places ", S1),
    string_concat(S1, T, MSG),
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 200,

    insects:can_move_any_of_the_insects(Player_id, Placed_neighbors),
    
    findall(([I,V,M]), (member(PN, Placed_neighbors), insects:get_last_insect(PN, I), best_move(I, M), [_,_,_,H,_,_]=I ,calculate_hexagon_total_value(H, V)), Moves),

    findall([I1,V2,H2], (member([I1,V1,(V2:H2)], Moves) ), L),

    tell('log1'),
    write(L),
    told,
    
    L \= [],

    max_move_value2(L, Max),
    
    [[Type, Id, _, Hex_Ori, true, Lvl],_,Hex_End] = Max,
    
    insects:move_insect(Type, Id, Player_id, Lvl, Hex_Ori, Hex_End, _),

    Status_Code = 200,
    string_concat(Name, " moves ", S1),
    string_concat(S1, Type, MSG),
    !.

analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 200,

    insects:can_move_any_of_the_insects(Player_id, Placed_neighbors),
    
    findall(([I,V,M]), (member(PN, Placed_neighbors), insects:get_last_insect(PN, I), best_move(I, M), [_,_,_,H,_,_]=I ,calculate_hexagon_total_value(H, V)), Moves),

    findall([I1,V2,H2], (member([I1,V1,(V2:H2)], Moves) ), L),

    tell('log1'),
    write(L),
    told,
    
    L == [],

    Status_Code = 200,
    string_concat(Name, " cant't play ", MSG),
    !.


analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 400,
    insects:get_placed_neighbors_of_player_hex(Player_id, Hex1, Placed_neighbors),
    
    findall(([I,V,M]), (member(PN, Placed_neighbors), insects:get_last_insect(PN, I), best_move(I, M), [_,_,_,H,_,_]=I ,calculate_hexagon_total_value(H, V)), Moves),

    findall([I1,V2,H2], (member([I1,V1,(V2:H2)], Moves), V2 > V1 ), L),

    L \= [],

    max_move_value2(L, Max),
    
    [[Type, Id, _, Hex_Ori, true, Lvl],_,Hex_End] = Max,
    
    insects:move_insect(Type, Id, Player_id, Lvl, Hex_Ori, Hex_End, _),

    Status_Code = 200,
    string_concat(Name, " moves ", S1),
    string_concat(S1, Type, MSG),
    !.

analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 400,
    
    findall(Hex, (insect(_, _, Player_id, Hex, true, _)), Hexagons),
    
    findall(([I,V,M]), (member(PN, Hexagons), insects:get_last_insect(PN, I), best_move(I, M), [_,_,_,H,_,_]=I ,calculate_hexagon_total_value(H, V)), Moves),
    
    findall([I1,V2,H2], (member([I1,V1,(V2:H2)], Moves), V2 > V1 ), L),

    L \= [],
    
    max_move_value2(L, Max),
    
    [[Type, Id, _, Hex_Ori, true, Lvl],_,Hex_End] = Max,
    
    insects:move_insect(Type, Id, Player_id, Lvl, Hex_Ori, Hex_End, _),

    Status_Code = 200,
    string_concat(Name, " moves ", S1),
    string_concat(S1, Type, MSG),
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 400,

    insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Queen_bee_placed, InsectsPlacements, Placements),

    findall((Score1:Type), (member([Type, Id, Player_id, none, false, -1], InsectsPlacements), insects:score(Type, Player_id, Score), Score1 is -1*Score),L),

    max_move_value(L, (S:T)),
    
    length(Placements, Len_placements),
    random_between(1, Len_placements, Rd_placements),
    
    % length(InsectsPlacements, Len_insects_placements),
    % random_between(1, Len_insects_placements, Rd_insects_placements),
    
    element_at(Hex, Placements, Rd_placements),
    % element_at([Type|_], InsectsPlacements, Rd_insects_placements),
    
    insects:place_insect(Player_id, T, Hex, _),

    Status_Code=200,
    string_concat(Name, " places ", S1),
    string_concat(S1, T, MSG),
    !.
    
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 400,

    insects:can_move_any_of_the_insects(Player_id, Placed_neighbors),
    
    findall(([I,V,M]), (member(PN, Placed_neighbors), insects:get_last_insect(PN, I), best_move(I, M), [_,_,_,H,_,_]=I ,calculate_hexagon_total_value(H, V)), Moves),

    findall([I1,V2,H2], (member([I1,V1,(V2:H2)], Moves) ), L),

    tell('log2'),
    write(L),
    told,

    L \= [],

    max_move_value2(L, Max),
    
    [[Type, Id, _, Hex_Ori, true, Lvl],_,Hex_End] = Max,
    
    insects:move_insect(Type, Id, Player_id, Lvl, Hex_Ori, Hex_End, _),

    Status_Code = 200,
    string_concat(Name, " moves ", S1),
    string_concat(S1, Type, MSG),
    !.
analize_type_of_play(Player_id, Name, Number_of_moves, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves > 2,
    insects:insect(queen_bee, Id1, Player_id, Hex1, true, Lvl1),
    insects:possible_moves(Player_id, queen_bee, Id1, Hex1, _, SC),
    SC == 400,

    insects:can_move_any_of_the_insects(Player_id, Placed_neighbors),
    
    findall(([I,V,M]), (member(PN, Placed_neighbors), insects:get_last_insect(PN, I), best_move(I, M), [_,_,_,H,_,_]=I ,calculate_hexagon_total_value(H, V)), Moves),

    findall([I1,V2,H2], (member([I1,V1,(V2:H2)], Moves) ), L),

    tell('log2'),
    write(L),
    told,

    L == [],

    Status_Code = 200,
    string_concat(Name, " cant't play ", MSG),
    !.


best_move(Insect, M):-
    [Type, Id, PlayerId, Hex, Placed, Lvl] = Insect,

    insects:possible_moves(PlayerId, Type, Id, Hex, Moves, SC),
    SC == 200,
    Moves \= [],    

    findall((V:M), (member(M, Moves), calculate_play_value(Insect, M, V)), Values),
    max_move_value(Values, M),!.
best_move(_, _):-
    !,
    fail.

calculate_hexagon_total_value(Hex, V):-
    hexagon:axial_neighbors(Hex, AN),
    sum_hexagons_value(AN, V),!.

calculate_play_value(Insect, HexagonEnd, Value):-
    [Type, Id, Player_id, Hex, Placed, Lvl] = Insect,
    
    insects:move_insect(Type, Id, Player_id, Lvl, Hex, HexagonEnd, _),
    hexagon:axial_neighbors(HexagonEnd, AN),
    sum_hexagons_value(AN, Value),
    insects:insect(Type, Id, Player_id,HexagonEnd, true, Lvl1),
    insects:move_insect(Type, Id, Player_id, Lvl1, HexagonEnd, Hex, _).

calculate_hexagon_value(Hex, 0):-
    insects:is_an_empty_hex(Hex),!.
calculate_hexagon_value(Hex, Value):-
    insects:get_last_insect(Hex, I),
    [Type, Id, Pid, Hex, true, Lvl] = I,
    insects:score(Type, Pid, V),
    insects:possible_moves(Pid, Type, Id, Hex, Moves, Status_Code),
    Status_Code == 200,
    Moves \= [],
    Value = V,
    !.
calculate_hexagon_value(Hex, Value):-
    insects:get_last_insect(Hex, I),!,
    [Type, Id, Pid, Hex, true, Lvl] = I,
    insects:score(Type, Pid, V),
    insects:possible_moves(Pid, Type, Id, Hex, Moves, Status_Code),
    Value is 2*V,
    !.

sum_hexagons_value([], 0):-!.
sum_hexagons_value([H|T], S):-
    calculate_hexagon_value(H, S1),
    sum_hexagons_value(T, S2),
    S is S1 + S2.

max((X1:Y1),(X2:Y2),(X1:Y1)):-
    X1 >=X2,!.
max((X1:Y1),(X2:Y2),(X2:Y2)):-!.

max2([X1,Y1,Z1],[X2,Y2,Z2],[X1,Y1,Z1]):-
    Y1 >=Y2,!.
max2([X1,Y1,Z1],[X2,Y2,Z2],[X2,Y2,Z2]):-!.


max_move_value([], (0,0)):-!.
max_move_value([(X:Y)], (X:Y)):-!.
max_move_value([(X:Y)|T], V):-
    max_move_value(T, V1),
    max((X:Y), V1, V),!.

max_move_value2([], (0,0,0)):-!.
max_move_value2([[X,Y,Z]], [X,Y,Z]):-!.
max_move_value2([[X,Y,Z]|T], V):-
    max_move_value2(T, V1),
    max2([X,Y,Z], V1, V),!.
