%% @author Tobias Rodaebel
%% @doc Web Socket handler behaviour.

-module(websocket_handler).

%% API
-export([behaviour_info/1, loop/1, handle_message/1, handle_close/1]).

%% @doc Defines the Web Socket handler behaviour.
%% @spec behaviour_info(callbacks) -> [] | undefined
behaviour_info(callbacks) ->
    [{init_handler, 0}, {handle_message, 1}, {handle_close, 1}];
behaviour_info(_Other) ->
    undefined.

%% @doc Server main loop.
%% @spec loop({Handler, Type, Socket}) -> void()
loop({Handler, Type, Socket}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Handler:handle_message({Type, Socket, Data}),
            loop({Handler, message, Socket});
        {error, closed} ->
            Handler:handle_close(Socket);
        {error, Reason} ->
            Handler:handle_close(Socket),
            error_logger:error_report({?MODULE, loop, Reason})
    end.

%% @doc Handles Web Socket message.
%% @spec handle_message(Msg) -> void()
handle_message(_Msg) ->
    noreply.

%% @doc Handles closed Web Socket.
%% @spec handle_close(Socket) -> void()
handle_close(_Socket) ->
    noreply.
