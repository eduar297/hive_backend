% ##################################################################################
% --------------------------------------SERVER--------------------------------------
% ##################################################################################

% --------------------------------------EXPORTS--------------------------------------
% ...

% --------------------------------------MODULES--------------------------------------
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- consult(game), import(game).

% --------------------------------------URL HANDLERS--------------------------------------
:- http_handler('/hive_api/ping', handle_request_ping_pong, []).
:- http_handler('/hive_api/insect/get_possible_placements', handle_request_get_possible_placements, []).
:- http_handler('/hive_api/insect/place_insect', handle_request_place_insect, []).
:- http_handler('/hive_api/game/game_stats', handle_request_game_stats, []).

% --------------------------------------METHODS--------------------------------------
% ping_pong | Tester server
ping_pong(_{msg:Msg}) :-
    Msg = "pong".

% Place_insect
placeInsect(_{type:Type, hexagon:Hexagon}, _{msg:MSG}) :-
    string_to_atom(Type, Type_atom),
    (not(compound(Hexagon)); not(atom(Type_atom))),
    MSG = "Wrong params",
    !.
placeInsect(_{type:_, hexagon:Hexagon}, _{msg:MSG}) :-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, _),
    game:insects:possible_placements(Player_id, Number_of_moves, Placements),
    not(member(Hexagon, Placements)),
    MSG = "Wrong placement",
    !.
placeInsect(_{type:Type, hexagon:_}, _{msg:MSG}) :-
    string_to_atom(Type, Type_atom),
    not(game:insects:insect(Type_atom, _, _, _, _)),
    MSG = "Nonexistent insect",
    !.
placeInsect(_{type:Type, hexagon:Hexagon}, _{insect:Insect}) :-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:insects:place_insect(Player_id, Type_atom, Hexagon, Insect),
    game:increment_number_of_moves(Player_id),
    game:next_player().

% Get possible placements
getPossiblePlacements(_{placements:Placements}) :-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, _),
    game:insects:possible_placements(Player_id, Number_of_moves, Placements).

% --------------------------------------Request Handlers--------------------------------------
% Handle ping pong
handle_request_ping_pong(_) :-
    ping_pong(Res),
    reply_json_dict(Res).

% Handle place insect
handle_request_place_insect(Req) :-
    http_read_json_dict(Req, Query),
    placeInsect(Query, Res),
    reply_json_dict(Res).

% Handle get possible placements
handle_request_get_possible_placements(_) :-
    getPossiblePlacements(Placements),
    Res = Placements,
    reply_json_dict(Res).

% Handle game stats
handle_request_game_stats(_):-
    game:current_player(Current_player_id),
    game:player(p1, Name_p1, Number_of_moves_p1, Queen_bee_placed_p1),
    game:player(p2, Name_p2, Number_of_moves_p2, Queen_bee_placed_p2),

    game:insects:all_insects(_, _, p1, _, no, Non_placed_insects_p1),
    game:insects:all_insects(_, _, p2, _, no, Non_placed_insects_p2),

    game:insects:all_insects(_, _, _, _, yes, Placed_insects),
    

    Players_info =
    _{
        p1:_{
            id:p1, name:Name_p1,    
            number_of_moves:Number_of_moves_p1,
            queen_bee_placed:Queen_bee_placed_p1,
            non_placed_insects: Non_placed_insects_p1
            },
        p2:_{
            id:p2, name:Name_p2,
            number_of_moves:Number_of_moves_p2,
            queen_bee_placed:Queen_bee_placed_p2,
            non_placed_insects: Non_placed_insects_p2
            }
    },

    Hive = Placed_insects,

    Res = _{current_player_id:Current_player_id, players_info:Players_info, hive:Hive},
    reply_json_dict(Res).
% --------------------------------------Start server--------------------------------------
start_server(Port) :-
    game:init_game(),
    http_server(http_dispatch, [port(Port)]).

% --------------------------------------Initialization--------------------------------------
:- initialization(start_server(3030), program).