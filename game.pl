% ################################################################################
% --------------------------------------GAME--------------------------------------
% ################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(game, [player/4, current_player/1, init_game/0, reset_game/0, next_player/0]).

% --------------------------------------MODULES--------------------------------------
:-consult(insects), import(insects).

% --------------------------------------DYNAMICS--------------------------------------
:-dynamic player/4, current_player/1.
% player(Id, Name, Number_of_moves, Queen_bee_placed) | Player Info | p1 is the first player
% current_player(Id)

% --------------------------------------METHODS--------------------------------------
% init game
init_game():-
    assert(player(p1, player_1, 0, no)),
    assert(player(p2, player_2, 0, no)),
    assert(current_player(p1)),
    insects:init_insects().

% reset game
reset_game():-
    retractall(player),
    retractall(current_player),
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
% --------------------------------------Initialization--------------------------------------
:- initialization(init_game()).