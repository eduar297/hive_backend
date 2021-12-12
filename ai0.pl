
% ##################################################################################
% -------------------------------ARTIFICIAL INTELLIGENCE EASY LEVEL ---------------------------
% ##################################################################################

% % --------------------------------------EXPORTS--------------------------------------
:- module(ai0, 
    [
        play_random/6
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(insects), import(insects).
:-consult(utils), import(utils).

% --------------------------------------DYNAMICS--------------------------------------
% ...

% --------------------------------------METHODS--------------------------------------

play_random(Player_id, Name, Number_of_moves, Queen_bee_placed, Status_Code, MSG):-
    insects:other_player(Player_id, Other_player_id),
    P1 = Player_id,
    P2 = Other_player_id,


    insects:all_insects(_, _, P1, _, false, _, _),
    insects:all_insects(_, _, P2, _, false, _, _),
    
    insects:all_insects(_, _, P1, _, true, _, _),
    insects:all_insects(_, _, P2, _, true, _, _),

    analize_type_of_play(P1, Number_of_moves, Queen_bee_placed, Type_of_play, InsectsPlacements, Placements, InsectMoves),
    [L,U] = Type_of_play,

    random_between(L, U, Rd),
    
    switch(Rd,
        [
            0: place(P1, Name, InsectsPlacements, Placements, MSG, Status_Code),
            1: move(P1, Name, InsectMoves, MSG, Status_Code),
            -1: (Status_Code=400, MSG="Can't play")
        ]).

% switch case {key: value=fun}
switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).

% Analize if can place and move, only one of the two optopons or neither.
analize_type_of_play(Player_id, Number_of_moves, Queen_bee_placed, Type_of_play, InsectsPlacements, Placements, InsectMoves):-
    % Can't play
    not(insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Queen_bee_placed, InsectsPlacements, Placements)),
    not(insects:can_move_any_of_the_insects(Player_id, InsectMoves)),
    Type_of_play = [-1,-1],!.
analize_type_of_play(Player_id, Number_of_moves, Queen_bee_placed, Type_of_play, InsectsPlacements, Placements, InsectMoves):-
    % Just place insect
    insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Queen_bee_placed, InsectsPlacements, Placements),
    not(insects:can_move_any_of_the_insects(Player_id, InsectMoves)),
    Type_of_play = [0,0],!.
analize_type_of_play(Player_id, Number_of_moves, Queen_bee_placed, Type_of_play, InsectsPlacements, Placements, InsectMoves):-
    % Just move insect
    not(insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Queen_bee_placed, InsectsPlacements, Placements)),
    insects:can_move_any_of_the_insects(Player_id, InsectMoves),
    Type_of_play = [1,1],!.
analize_type_of_play(Player_id, Number_of_moves, Queen_bee_placed, Type_of_play, InsectsPlacements, Placements, InsectMoves):-
    % Place or move
    insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Queen_bee_placed, InsectsPlacements, Placements),
    insects:can_move_any_of_the_insects(Player_id, InsectMoves),
    Type_of_play = [0,1],!.

place(Player_id, Name, InsectsPlacements, Placements, MSG, Status_Code):-
    length(Placements, Len_placements),
    random_between(1, Len_placements, Rd_placements),
    
    length(InsectsPlacements, Len_insects_placements),
    random_between(1, Len_insects_placements, Rd_insects_placements),

    
    element_at(Hex, Placements, Rd_placements),
    element_at([Type|_], InsectsPlacements, Rd_insects_placements),

    
    insects:place_insect(Player_id, Type, Hex, _),

    string_concat(Name, " places ", S1),
    string_concat(S1, Type, MSG),

    Status_Code = 200,
    !.

move(Player_id, Name, InsectMoves, MSG, Status_Code):-
    length(InsectMoves, Len_insect_moves),
    random_between(1, Len_insect_moves, Rd_insect_moves),
    element_at(InsectMove, InsectMoves, Rd_insect_moves),

    (Hexagon_Ori:PossibleMoves) = InsectMove,
    
    length(PossibleMoves, Len_possible_moves),
    random_between(1, Len_possible_moves, Rd_possible_moves),
    element_at(Hexagon_End, PossibleMoves, Rd_possible_moves),

    insects:get_last_insect(Hexagon_Ori, Insect),

    [Type, Id, _, _, true, Lvl] = Insect,

    insects:move_insect(Type, Id, Player_id, Lvl, Hexagon_Ori, Hexagon_End, _),

    string_concat(Name, " moves ", S1),
    string_concat(S1, Type, MSG),

    Status_Code=200,
    !.
    