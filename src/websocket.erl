%% @author Tobias Rodaebel
%% @copyright 2011 Tobias Rodaebel
%% @doc Web Socket Server main module.

-module(websocket).
-author('tobias.rodaebel@googlemail.com').

-export([start/0, stop/0]).

%% @doc Starts the server.
%% @spec start() -> ok | {error, Reason}
start() ->
    application:start(websocket).

%% @doc Stops the server.
%% @spec stop() -> ok
stop() ->
    application:stop(websocket).
