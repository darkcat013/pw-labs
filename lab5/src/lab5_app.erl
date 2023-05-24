-module(lab5_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = db:init(),
    {ok, Pid} = start_server(),
    ok = bot:init_bot(),
    {ok, Pid}.

stop(_State) ->
    ok.

start_server() ->
    Port = 88,
    Dispatch = cowboy_router:compile([{'_', [{"/", bot_request_handler, []}]}]),
    {ok, Pid} =
        cowboy:start_clear(http_server, [{port, Port}], #{env => #{dispatch => Dispatch}}),
    {ok, Pid}.
