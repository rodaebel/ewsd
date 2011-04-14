%% @author Tobias Rodaebel
%% @doc Web Socket Server.

-module(websocket_server).
-vsn("1.0.0").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Callbacks of the gen_server behaviour
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-export([accept_loop/1]).

-include("websocket.hrl").

-record(state, {ip, port, socket, handler, timeout=infinity}).

-define(SERVER, ?MODULE).

%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    {ok, Address} = application:get_env(websocket, ip),
    {ok, Port} = application:get_env(websocket, port),
    {ok, Module} = application:get_env(websocket, handler),
    {ok, Timeout} = application:get_env(websocket, timeout),
    State = #state{ip=Address, port=Port, handler=Module, timeout=Timeout},
    gen_server:start_link({local, ?SERVER}, ?MODULE, State, []).

%% @private
%% @doc Initializes the server.
%% @spec init(Args) -> {ok, State} | {stop, Reason}
init(State = #state{ip=Address, port=Port, handler=Handler, timeout=Timeout}) ->
    ok = Handler:init_handler(),
    Options = [binary, {ip, Address}, {active, false}, {reuseaddr, true},
               {packet, 0}],
    case gen_tcp:listen(Port, Options) of
        {ok, Socket} ->
            error_logger:info_msg("~p Listening on port ~p~n", [self(), Port]),
            S = State#state{socket=Socket, timeout=Timeout},
            {ok, accept(S)};
        {error, Reason} ->
            error_logger:error_report({?MODULE, tcp_listen, Reason}),
            {stop, {?MODULE, tcp_listen, Reason}}
    end.

%% @private
%% @doc Accepts TCP connections.
%% @spec accept_loop({Server, Socket, Handler, Timeout}) -> any()
accept_loop({Server, Socket, Handler, Timeout}) ->
    {ok, S} = gen_tcp:accept(Socket),
    gen_server:cast(Server, {accepted, self()}),
    websocket_handler:loop({Handler, handshake, S, Timeout}).

%% @private
%% @doc Spawns a new child process for the new connection.
%% @spec accept(State) -> State
accept(State = #state{socket=Socket, handler=Handler, timeout=Timeout}) ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, accept_loop, [{self(), Socket, Handler, Timeout}]),
    State.

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) -> {noreply, State}
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handles cast messages.
%% @spec handle_cast(Msg, State) -> {noreply, State}
handle_cast({accepted, _Pid}, State = #state{}) ->
    {noreply, accept(State)};
handle_cast(Msg, State = #state{handler=Handler}) ->
    websocket_handler:push(Handler, Msg),
    {noreply, State}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State}
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
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
