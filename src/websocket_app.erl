%% @author Tobias Rodaebel <tobias.rodaebel@googlemail.com>
%% @copyright 2011 Tobias Rodaebel
%% @doc Web Socket Server Application.

-module(websocket_app).
-author('tobias.rodaebel@googlemail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% @doc Starts the application.
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
start(_StartType, _StartArgs) ->
    websocket_sup:start_link().

%% @doc Stops the application.
%% @spec stop(State) -> ok
stop(_State) ->
    ok.
