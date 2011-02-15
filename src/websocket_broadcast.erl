%% @author Tobias Rodaebel
%% @doc Web Socket broadcast handler.

-module(websocket_broadcast).

-behaviour(websocket_handler).

%% API
-export([init_handler/0, loop/1, handle_message/1]).

%% @doc Initializes the handler.
%% @spec init_handler() -> ok
init_handler() ->
    ets:new(clients, [public, named_table, ordered_set]),
    ok.

%% @private
%% @doc Server main loop.
%% @spec loop({Type, Socket}) -> void()
loop({Type, Socket}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            handle_message({Type, Socket, Data});
        {error, closed} ->
            ets:delete_object(clients, {Socket}),
            error_logger:info_msg("~p Socket closed~n", [self()]);
        {error, Reason} ->
            error_logger:error_report({?MODULE, loop, Reason})
    end.

%% @private
%% @doc Handles Web Socket messages.
%% @spec handle_message({Type, Socket, Data}) -> void()
handle_message({handshake, Socket, Data}) ->
    Response = websocket_lib:process_handshake(Data),
    ets:insert_new(clients, {Socket}),
    gen_tcp:send(Socket, Response),
    loop({message, Socket});
handle_message({message, Socket, Data}) ->
    Frame = [0, Data, 255],
    ets:foldl(fun({S}, _Acc) -> gen_tcp:send(S, Frame) end, notused, clients),
    loop({message, Socket}).
