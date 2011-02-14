%% @author Tobias Rodaebel <tobias.rodaebel@googlemail.com>
%% @copyright 2011 Tobias Rodaebel
%% @doc Web Socket Server.

-module(websocket_server).
-author("tobias.rodaebel@googlemail.com").
-vsn("1.0.0").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Callbacks of the gen_server behaviour
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-export([accept_loop/1, loop/1]).

-include("websocket.hrl").

-define(SERVER, ?MODULE). 

%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    {ok, Address} = application:get_env(ip),
    {ok, Port} = application:get_env(port),
    Loop = {?SERVER, loop},
    State = #server_state{ip=Address, port=Port, loop=Loop},
    gen_server:start_link({local, ?SERVER}, ?MODULE, State, []).

%% @private
%% @doc Initializes the server.
%% @spec init(Args) -> {ok, State} | {stop, Reason}
init(State = #server_state{ip=Address, port=Port}) ->
    Options = [binary, {ip, Address}, {active, false}, {reuseaddr, true},
               {packet, 0}],
    case gen_tcp:listen(Port, Options) of
        {ok, Socket} ->
            error_logger:info_msg("~p Listening on port ~p~n", [self(), Port]),
            NewState = State#server_state{socket = Socket},
            {ok, accept(NewState)};
        {error, Reason} ->
            error_logger:error_report({?MODULE, tcp_listen, Reason}),
            {stop, {?MODULE, tcp_listen, Reason}}
    end.

accept_loop({Server, Socket, {M, F}}) ->
    {ok, S} = gen_tcp:accept(Socket),
    error_logger:info_msg("~p Socket connected~n", [self()]),
    gen_server:cast(Server, {accepted, self()}),
    M:F({handshake, S}).
    
accept(State = #server_state{socket=Socket, loop=Loop}) ->
    spawn(?MODULE, accept_loop, [{self(), Socket, Loop}]),
    State.

%% @doc Server main loop.
loop({Type, Socket}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            handle_message({Type, Socket, Data});
        {error, closed} ->
            error_logger:info_msg("~p Socket closed~n", [self()]);
        {error, Reason} ->
            error_logger:error_report({?MODULE, loop, Reason})
    end.

%% @private
%% @doc Handles Web Socket messages.
%% @spec handle_message({Type, Socket, Data}) -> void()
handle_message({handshake, Socket, Data}) ->
    Response = websocket_lib:process_handshake(Data),
    gen_tcp:send(Socket, Response),
    loop({message, Socket});
handle_message({message, Socket, Data}) ->
    gen_tcp:send(Socket, [0, Data, 255]),
    loop({message, Socket}).

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) -> {noreply, State}
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handles cast messages.
%% @spec handle_cast(Msg, State) -> {noreply, accept(State)}
handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State}
handle_info(Info, State) ->
    error_logger:info_msg("~p handle_info(~p, ~p)~n", [self(), Info, State]),
    {noreply, State}.

%% @private
%% @doc Performs cleanup on termination.
%% @spec terminate(Reason, State) -> ok
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, Library, Extra) -> {ok, Library}
code_change(_OldVsn, Library, _Extra) ->
    {ok, Library}.
