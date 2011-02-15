%% @author Tobias Rodaebel
%% @doc Web Socket Server main module.

-module(websocket).

-export([start/0, stop/0]).

%% @doc Starts the server.
%% @spec start() -> ok | {error, Reason}
start() ->
    application:start(websocket).

%% @doc Stops the server.
%% @spec stop() -> ok
stop() ->
    application:stop(websocket).
