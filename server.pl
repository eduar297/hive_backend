% --------------------------------------MODULES--------------------------------------
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- consult(game), import(game).

% --------------------------------------EXPORTS--------------------------------------
% -

% --------------------------------------URL HANDLERS--------------------------------------
:- http_handler('/hive_api/say_hi', handle_request_say_hi, []).
:- http_handler('/hive_api/insect/get_possible_placements', handle_request_get_possible_placements, []).
:- http_handler('/hive_api/insect/place_insect', handle_request_place_insect, []).


% --------------------------------------METHODS--------------------------------------
% Say hi
sayHi(_{msg:Msg}) :-
    Msg = "Hi!!!".

% Place_insect
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
% Handle say hi
handle_request_say_hi(_) :-
    sayHi(Res),
    reply_json_dict(Res).

% % Handle place insect
handle_request_place_insect(Req) :-
    http_read_json_dict(Req, Query),
    placeInsect(Query, Res),
    reply_json_dict(Res).

% Handle get possible placements
handle_request_get_possible_placements(_) :-
    getPossiblePlacements(Placements),
    Res = Placements,
    reply_json_dict(Res).

% --------------------------------------Start server--------------------------------------
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% --------------------------------------Initialization--------------------------------------
:- initialization(server(3030), program).