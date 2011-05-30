%% @author Tobias Rodaebel
%% @doc Web Socket handler behaviour.

-module(websocket_handler).

%% API
-export([behaviour_info/1, handle_message/1, handle_push/1,
         handle_close/1, loop/1, push/2]).

%% @doc Defines the Web Socket handler behaviour.
%% @spec behaviour_info(callbacks) -> [] | undefined
behaviour_info(callbacks) ->
    [{init, 0}, {handle_message, 1}, {handle_push, 1}, {handle_close, 1}];
behaviour_info(_Other) ->
    undefined.

%% @doc Handles Web Socket message.
%% @spec handle_message(Msg) -> any()
handle_message(_Msg) ->
    noreply.

%% @doc Handles push messages.
%% @spec handle_push(Msg) -> any()
handle_push(_Msg) ->
    noreply.

%% @doc Handles closed Web Socket.
%% @spec handle_close(Socket) -> any()
handle_close(_Socket) ->
    noreply.

%% @doc Server main loop.
%% @spec loop({Handler, Type, Socket, Timeout}) -> any()
loop({Handler, Type, Socket, Timeout}) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, Data} ->
            L = size(Data) - 2,
            case Data of
                <<0,Bin:L/binary,255>> ->
                    Handler:handle_message({Type, Socket, Bin});
                Any ->
                    Handler:handle_message({Type, Socket, Any})
            end,
            loop({Handler, message, Socket, Timeout});
        {error, closed} ->
            Handler:handle_close({closed, Socket});
        {error, timeout} ->
            Handler:handle_close({timeout, Socket});
        {error, Reason} ->
            Handler:handle_close({error, Socket}),
            error_logger:error_report({?MODULE, loop, Reason})
    end.

%% @doc Pushes messages throught the Web Socket handler.
%% @spec push(Handler, Msg) -> any()
push(Handler, Msg) ->
    Handler:handle_push(Msg).
