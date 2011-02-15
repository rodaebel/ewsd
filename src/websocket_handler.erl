%% @author Tobias Rodaebel
%% @doc Web Socket handler behaviour.

-module(websocket_handler).

%% API
-export([behaviour_info/1]).

%% @doc Defines the Web Socket handler behaviour.
%% @spec behaviour_info(callbacks) -> [] | undefined
behaviour_info(callbacks) ->
    [{init_handler, 0}, {loop, 1}, {handle_message, 1}];
behaviour_info(_Other) ->
    undefined.
