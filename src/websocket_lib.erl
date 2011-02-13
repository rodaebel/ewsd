%% @author Tobias Rodaebel <tobias.rodaebel@googlemail.com>
%% @copyright 2011 Tobias Rodaebel
%% @doc Web Socket Library.
%% @reference http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-76

-module(websocket_lib).
-author("tobias.rodaebel@googlemail.com").

%% API
-export([process_handshake/1]).

-include("websocket.hrl").

-include_lib("eunit/include/eunit.hrl").

%% @doc Processes the client's handshake request.
%% @spec process_handshake(Bin) -> Response
process_handshake(Bin) ->
    Data = binary_to_list(Bin),
    [Body|Head] = lists:reverse(string:tokens(Data, "\r\n")),
    {Key1, Key2, Origin, Host} = parse_header(lists:reverse(Head)),
    Key3 = list_to_binary(Body),
    Challenge = calc_challenge(Key1, Key2, Key3),
    Response = [
        "HTTP/1.1 101 WebSocket Protocol Handshake\r\n",
        "Upgrade: WebSocket\r\nConnection: Upgrade\r\n",
        "Sec-WebSocket-Origin: ", Origin, "\r\n",
        "Sec-WebSocket-Location: ws://", Host, "/\r\n",
        "Sec-WebSocket-Protocol: chat\r\n\r\n", Challenge],
    Response.

%% Pivate API

%% @private
%% @doc Parses header from the client's handshake request.
%% @spec parse_header(Head) -> {Key1, Key2, Origin, Host}
parse_header(Head) ->
    Key1 = get_header_value("Sec-WebSocket-Key1", Head),
    Key2 = get_header_value("Sec-WebSocket-Key2", Head),
    Origin = get_header_value("Origin", Head),
    Host = get_header_value("Host", Head),
    {Key1, Key2, Origin, Host}.

get_header_value(String, [H|T]) ->
    case string:str(H, string:concat(String, ": ")) of
        0 -> get_header_value(String, T); 
        _Pos -> string:substr(H, string:len(String) + 3)
    end.

%% @private
%% @doc Calcualte the handshake challenge.
%% @spec calc_challenge(Key1, Key2, Key3) -> Result
calc_challenge(Key1, Key2, Key3) ->
    {Digits1, []} = string:to_integer(concat_digits(Key1)),
    {Digits2, []} = string:to_integer(concat_digits(Key2)),
    Skey1 = Digits1 div count_spaces(Key1),
    Skey2 = Digits2 div count_spaces(Key2),
    Challenge = erlang:md5(<<Skey1:32/big-unsigned-integer,
                             Skey2:32/big-unsigned-integer,
                             Key3/binary>>),
    Challenge.

%% @private
%% @doc Concatenates digits 0-9 of a string.
%% @spec concat_digits(String) -> List
concat_digits(String) -> [A || A <- String, A =< 57, A >= 48].

%% @private
%% @doc Counts number of spaces in a string.
%% @spec count_spaces(String) -> Integer
count_spaces(String) -> string:len([ A || A <- String, A =:= 32]).
