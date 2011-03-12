%% @author Tobias Rodaebel
%% @doc Web Socket broadcast handler.

-module(websocket_broadcast).

-behaviour(websocket_handler).

%% API
-export([init_handler/0, handle_message/1, handle_close/1]).

-export([receiver/0]).

%% @doc Initializes the handler.
%% @spec init_handler() -> ok
init_handler() ->
    ets:new(clients, [public, named_table, ordered_set]).

%% @doc Handles Web Socket messages.
%% @spec handle_message({Type, Socket, Data}) -> any()
handle_message({handshake, Socket, Data}) ->
    {ok, Response, Path} = websocket_lib:process_handshake(Data),
    ets:insert_new(clients, {Socket, Path}),
    gen_tcp:send(Socket, Response),
    error_logger:info_msg("~p Socket connected (~s)~n", [self(), Path]);
handle_message({message, _Socket, Bin}) ->
    broadcast(Bin).

%% @doc Handles closed Web Socket.
%% @spec handle_close(Socket) -> any()
handle_close(Socket) ->
    ets:match_delete(clients, {Socket, '_'}),
    error_logger:info_msg("~p Socket closed~n", [self()]).

%% @doc A simple receiver.
%% @spec receiver() -> any()
receiver() ->
    receive
        {accelerometer, [X,Y,Z]} ->
            broadcast(io_lib:format("{\"x\":~f,\"y\":~f,\"z\":~f}", [X,Y,Z]));
        {scale, [Scale]} ->
            broadcast(io_lib:format("{\"s\":~f}", [Scale]));
        Any ->
            error_logger:info_msg("~p ~p~n", [self(), Any])
    end,
    receiver().

%% @private
%% @doc Broadcasts Web Socket messages.
%% @spec broadcast(Data) -> ok
broadcast(Data) when is_binary(Data) ->
    broadcast_frame([0, Data, 255]);
broadcast(Data) ->
    broadcast_frame([0, list_to_binary(lists:flatten(Data)), 255]).

%% @private
%% @doc Broadcasts data frame to the Web Socket clients.
%% @spec broadcast_frame(F) -> ok
broadcast_frame(F) ->
    ets:foldl(fun({S, _}, _Acc) -> gen_tcp:send(S, F) end, notused, clients).
