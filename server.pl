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
:- http_handler('/hive_api/insect/get_possible_moves', handle_request_get_possible_moves, []).
:- http_handler('/hive_api/insect/place_insect', handle_request_place_insect, []).
:- http_handler('/hive_api/game/game_stats', handle_request_game_stats, []).
:- http_handler('/hive_api/game/reset_game', handle_request_reset_game, []).
% --------------------------------------METHODS--------------------------------------
% ping_pong | Tester server
ping_pong(_{status_code:Status_Code, msg:Msg}) :-
    Status_Code = 200,
    Msg = "pong".

% Place_insect
placeInsect(_{type:Type, hexagon:Hexagon}, _{status_code:Status_Code, msg:MSG}) :-
    string_to_atom(Type, Type_atom),
    (not(compound(Hexagon)); not(atom(Type_atom))),
    Status_Code = 400,
    MSG = "Wrong params!",
    !.
placeInsect(_{type:_, hexagon:Hexagon}, _{status_code:Status_Code, msg:MSG}) :-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, _),
    game:insects:possible_placements(Player_id, Number_of_moves, Placements),
    not(member(Hexagon, Placements)),
    Status_Code = 400,
    MSG = "Wrong placement",
    !.
placeInsect(_{type:Type, hexagon:_}, _{status_code:Status_Code, msg:MSG}) :-
    string_to_atom(Type, Type_atom),
    not(game:insects:insect(Type_atom, _, _, _, _)),
    Status_Code = 400,
    MSG = "Nonexistent insect",
    !.
placeInsect(_{type:Type, hexagon:Hexagon}, _{status_code:Status_Code, insect:Insect}) :-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    Type_atom == queen_bee,
    game:insects:place_insect(Player_id, Type_atom, Hexagon, Insect),
    game:place_queen_bee(Player_id),
    game:increment_number_of_moves(Player_id),
    game:next_player(),
    Status_Code = 200,
    !.
placeInsect(_{type:Type, hexagon:Hexagon}, _{status_code:Status_Code, insect:Insect}) :-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:insects:place_insect(Player_id, Type_atom, Hexagon, Insect),
    game:increment_number_of_moves(Player_id),
    game:next_player(),
    Status_Code = 200.

% Get possible placements
getPossiblePlacements(_{type:Type}, _{status_code:Status_Code, placements:Placements}):-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, Queen_bee_placed),
    Queen_bee_placed == false,
    Number_of_moves == 3,
    Type_atom == queen_bee,
    game:insects:possible_placements(Player_id, Number_of_moves, Placements),
    Status_Code = 200,
    !.
getPossiblePlacements(_, _{status_code:Status_Code, msg:MSG}):-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, Queen_bee_placed),
    Queen_bee_placed == false,
    Number_of_moves == 3,
    Status_Code = 400,
    MSG = "4th move: you have to add your queen!",
    !.
getPossiblePlacements(_, _{status_code:Status_Code, placements:Placements}) :-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, _),
    game:insects:possible_placements(Player_id, Number_of_moves, Placements),
    Status_Code = 200.

% Get possible moves
getPossibleMoves(_{type:Type, hexagon:Hexagon}, _{status_code:Status_Code, moves:Moves}):-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:player(Player_id, _, _, _),
    game:insects:possible_moves(Player_id, Type_atom, Hexagon, Moves, Status_Code).

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
handle_request_get_possible_placements(Req) :-
    http_read_json_dict(Req, Query),
    getPossiblePlacements(Query, Placements),
    Res = Placements,
    reply_json_dict(Res).

% handle get possible moves
handle_request_get_possible_moves(Req):-
    http_read_json_dict(Req, Query),
    getPossibleMoves(Query, Res),
    reply_json_dict(Res).

% Handle game stats
handle_request_game_stats(_):-
    game:current_player(Current_player_id),
    game:player(p1, Name_p1, Number_of_moves_p1, Queen_bee_placed_p1),
    game:player(p2, Name_p2, Number_of_moves_p2, Queen_bee_placed_p2),

    game:insects:all_insects(_, _, p1, _, false, Non_placed_insects_p1),
    game:insects:all_insects(_, _, p2, _, false, Non_placed_insects_p2),

    game:insects:all_insects(_, _, _, _, true, Placed_insects),
    

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

    Status_Code = 200,

    Res = _{status_code:Status_Code, current_player_id:Current_player_id, players_info:Players_info, hive:Hive},
    reply_json_dict(Res).

% handle reset game
handle_request_reset_game(_):-
    game:reset_game(),
    Res = _{msg:"ok"},
    reply_json_dict(Res).
% --------------------------------------Start server--------------------------------------
start_server(Port) :-
    game:init_game(),
    http_server(http_dispatch, [port(Port)]).

% --------------------------------------Initialization--------------------------------------
:- initialization(start_server(3031), program).