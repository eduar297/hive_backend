% ################################################################################
% --------------------------------------GAME--------------------------------------
% ################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(game,
    [player/4,
    current_player/1,
    init_game/0,
    reset_game/0, 
    next_player/0,
    increment_number_of_moves/1,
    place_queen_bee/1
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(insects), import(insects).

% --------------------------------------DYNAMICS--------------------------------------
:-dynamic player/4, current_player/1.
% player(Id, Name, Number_of_moves, Queen_bee_placed) | Player Info | p1 is the first player and p2 is the second
% current_player(Id) | The current player' is

% --------------------------------------METHODS--------------------------------------
% init game
init_game():-
    assert(player(p1, 'Player 1', 0, false)),
    assert(player(p2, 'Player 2', 0, false)),
    assert(current_player(p1)),
    insects:init_insects().

% increment number of moves(Player_id):-
increment_number_of_moves(Player_id):-
    player(Player_id, Name, Number_of_moves, Queen_bee_placed),
    Number_of_moves1 is Number_of_moves + 1,
    retract(player(Player_id, Name, Number_of_moves, Queen_bee_placed)),
    assert(player(Player_id, Name, Number_of_moves1, Queen_bee_placed)).

% reset game
reset_game():-
    retractall(player(_,_,_,_)),
    retractall(current_player(_)),
    retractall(insects:insect(_,_,_,_,_,_)),
    init_game().

% next player
next_player():-
    current_player(P),
    P == p1,
    retract(current_player(p1)),
    assert(current_player(p2)),!.
next_player():-
    current_player(P),
    P == p2,
    retract(current_player(p2)),
    assert(current_player(p1)).

% place queen bee 
place_queen_bee(Player_id):-
    player(Player_id, Name, Number_of_moves, Queen_bee_placed),
    retract(player(Player_id, Name, Number_of_moves, Queen_bee_placed)),
    assert(player(Player_id, Name, Number_of_moves, true)).