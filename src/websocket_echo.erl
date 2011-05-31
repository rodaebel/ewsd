%% @author Tobias Rodaebel
%% @doc Web Socket echo handler.

-module(websocket_echo).

-behaviour(websocket_handler).

%% API
-export([init/0, handle_message/1, handle_push/1, handle_close/1]).

%% @doc Initializes the handler.
%% @spec init() -> ok
init() ->
    ok.

%% @doc Handles Web Socket messages.
%% @spec handle_message({Type, Socket, Data}) -> any()
handle_message({handshake, Socket, Data}) ->
    {ok, Response, _Path} = websocket_lib:process_handshake(Data),
    gen_tcp:send(Socket, Response);
handle_message({message, Socket, Bin}) ->
    gen_tcp:send(Socket, [0, Bin, 255]).

%% @doc Handles push messages.
%% @spec handle_push(Msg) -> any()
handle_push(_Msg) ->
    noreply.

%% @doc Handles closed Web Socket.
%% @spec handle_close(Msg) -> any()
handle_close(_Msg) ->
    ok.
