%% @author Tobias Rodaebel
%% @doc Web Socket broadcast handler.

-module(websocket_broadcast).

-behaviour(websocket_handler).

%% API
-export([init_handler/0, handle_message/1, handle_close/1]).

-export([callback/1]).

-define(KARAJAN_SERVER, {karajan_server, karajan@localhost}).

%% @doc Initializes the handler.
%% @spec init_handler() -> ok
init_handler() ->
    ets:new(clients, [public, named_table, ordered_set]),
    {ok, callback}.

%% @doc Handles Web Socket messages.
%% @spec handle_message({Type, Socket, Data}) -> any()
handle_message({handshake, Socket, Data}) ->
    {ok, Response, Path} = websocket_lib:process_handshake(Data),
    ets:insert_new(clients, {Socket, Path}),
    gen_tcp:send(Socket, Response),
    error_logger:info_msg("~p Socket connected (~s)~n", [self(), Path]);
handle_message({message, _Socket, Bin}) ->
    try json_parser:dvm_parser(Bin) of
        {ok,{struct,[{_,Scale}]},_} ->
            gen_server:cast(?KARAJAN_SERVER, {message, "/1/scale", [Scale]})
    catch
        _ ->
            error_logger:info_msg("~p JSON expected~n", [self()])
    end,
    broadcast(Bin).

%% @doc Handles closed Web Socket.
%% @spec handle_close(Socket) -> any()
handle_close(Socket) ->
    ets:match_delete(clients, {Socket, '_'}),
    error_logger:info_msg("~p Socket closed~n", [self()]).

%% @doc The optional callback method.
%% @spec callback() -> any()
callback({accelerometer, [X,Y,Z]}) ->
    broadcast(io_lib:format("{\"x\":~f,\"y\":~f,\"z\":~f}", [X,Y,Z]));
callback({scale, [Scale]}) ->
    broadcast(io_lib:format("{\"s\":~f}", [Scale])).

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
