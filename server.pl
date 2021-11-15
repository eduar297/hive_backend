% --------------------------------------MODULES--------------------------------------
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- consult(game), import(game).

% --------------------------------------EXPORTS--------------------------------------
% -

% --------------------------------------URL HANDLERS--------------------------------------
:- http_handler('/hive_api/say_hi', handle_request_say_hi, []).
:- http_handler('/hive_api/hex/get_possible_placements', handle_request_get_possible_placements, []).


% --------------------------------------METHODS--------------------------------------
% Say hi
sayHi(_{msg:Msg}) :-
    Msg = "Hi!!!".

% Get possible placements
% addHexagon(_{q:Q, r:R, tokens:Tokens}, _{hexagon:Hexagon}) :-
%     findall(A, (member(T, Tokens), string_to_atom(T, A)), Atoms),
%     hexagon:add_hexagon(Q, R, Atoms, Hexagon).
    
% Get possible placements
get_possible_placements(_{placements:Placements}) :-
    game:current_player(Player_id),
    game:player(Player_id, Name, Number_of_moves, Queen_bee_placed),
    game:insects:possible_placements(Player_id, Number_of_moves, Placements).

% --------------------------------------Request Handlers--------------------------------------
% Handle say hi
handle_request_say_hi(Req) :-
    sayHi(Res),
    reply_json_dict(Res).

% % Handle add hex
% handle_request_add_hexagon(Req) :-
%     http_read_json_dict(Req, Query),
%     addHexagon(Query, Hexagon),
%     Res = Hexagon,
%     reply_json_dict(Res).

% Handle get possible placements
handle_request_get_possible_placements(Req) :-
    get_possible_placements(Placements),
    reply_json_dict(Placements).

% --------------------------------------Start server--------------------------------------
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% --------------------------------------Initialization--------------------------------------
:- initialization(server(3030), program).