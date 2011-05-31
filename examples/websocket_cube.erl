%% @author Tobias Rodaebel
%% @doc Web Socket broadcast handler for the Cube Demo.

-module(websocket_cube).

-behaviour(websocket_handler).

%% API
-export([init/0, handle_message/1, handle_push/1, handle_close/1]).

-define(KARAJAN_SERVER, {karajan_server, karajan@localhost}).

%% @doc Initializes the handler.
%% @spec init() -> ok
init() ->
    ets:new(clients, [public, named_table, ordered_set]),
    ok.

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
            Message = binary_to_list(Bin),
            error_logger:info_msg("~p Received: ~s~n", [self(), Message])
    end,
    broadcast(Bin).

%% @doc Handles push messages.
%% @spec handle_push(Msg) -> any()
handle_push({accelerometer, [X,Y,Z]}) ->
    broadcast(io_lib:format("{\"x\":~f,\"y\":~f,\"z\":~f}", [X,Y,Z]));
handle_push({scale, [Scale]}) ->
    broadcast(io_lib:format("{\"s\":~f}", [Scale])).

%% @doc Handles closed Web Socket.
%% @spec handle_close(Msg) -> any()
handle_close({_, Socket}) ->
    ets:match_delete(clients, {Socket, '_'}),
    error_logger:info_msg("~p Socket closed~n", [self()]).

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