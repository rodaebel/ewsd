%% @author Tobias Rodaebel <tobias.rodaebel@googlemail.com>
%% @copyright 2011 Tobias Rodaebel
%% @doc Web Socket Server Supervisor.

-module(websocket_sup).
-author('tobias.rodaebel@googlemail.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% @doc Starts the supervisor.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initializes the supervisor.
%% @spec init(Args) -> {ok, {SupFlags, ChildSpecs}} | ignore | {error, Reason}
init([]) ->
    Server = {websocket_server, {websocket_server, start_link, []},
	      permanent, 2000, worker, [websocket_server]},
    {ok, {{one_for_one, 3, 10}, [Server]}}.
