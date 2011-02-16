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
%% @spec loop({Module, Type, Socket}) -> void()
loop({Module, Type, Socket}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Module:handle_message({Type, Socket, Data}),
            loop({Module, message, Socket});
        {error, closed} ->
            Module:handle_close(Socket);
        {error, Reason} ->
            Module:handle_close(Socket),
            error_logger:error_report({?MODULE, loop, Reason})
    end.

handle_message(_Msg) ->
    noreply.

handle_close(_Socket) ->
    noreply.
