%% @author Tobias Rodaebel
%% @doc Web Socket broadcast handler.

-module(websocket_broadcast).

-behaviour(websocket_handler).

%% API
-export([init_handler/0, handle_message/1, handle_close/1]).

%% @doc Initializes the handler.
%% @spec init_handler() -> ok
init_handler() ->
    ets:new(clients, [public, named_table, ordered_set]),
    ok.

%% @private
%% @doc Handles Web Socket messages.
%% @spec handle_message({Type, Socket, Data}) -> void()
handle_message({handshake, Socket, Data}) ->
    {ok, Response, Path} = websocket_lib:process_handshake(Data),
    ets:insert_new(clients, {Socket}),
    gen_tcp:send(Socket, Response),
    error_logger:info_msg("~p Socket connected (~s)~n", [self(), Path]);
handle_message({message, _Socket, Data}) ->
    Frame = [0, Data, 255],
    ets:foldl(fun({S}, _Acc) -> gen_tcp:send(S, Frame) end, notused, clients).

handle_close(Socket) ->
    ets:delete_object(clients, {Socket}),
    error_logger:info_msg("~p Socket closed~n", [self()]).
