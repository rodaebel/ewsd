%% @author Tobias Rodaebel
%% @doc Web Socket Library.
%% @reference http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-76

-module(websocket_lib).

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

    % Web Socket Handshake response
    "HTTP/1.1 101 WebSocket Protocol Handshake\r\n"
    "Upgrade: WebSocket\r\nConnection: Upgrade\r\n"
    "Sec-WebSocket-Origin: " ++ Origin ++ "\r\n"
    "Sec-WebSocket-Location: ws://" ++ Host ++ "/\r\n"
    "Sec-WebSocket-Protocol: chat\r\n\r\n" ++ Challenge.

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

get_header_value_test_() ->
    ?_assertEqual("test", get_header_value("Test", ["Foo: foo", "Test: test"])).

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

calc_challenge_test_() ->
    ?_assertEqual(<<249,214,215,98,131,120,84,118,243,69,68,91,56,235,101,215>>,
                  calc_challenge("89z sdhf 9sf", "fh siu is7f7t", <<"foo">>)).

%% @private
%% @doc Concatenates digits 0-9 of a string.
%% @spec concat_digits(String) -> List
concat_digits(String) -> [A || A <- String, A =< 57, A >= 48].

concat_digits_test_() ->
    [?_assertEqual([], concat_digits("Foo bar")),
     ?_assertEqual("82", concat_digits("hiu sd8 2  hfs")),
     ?_assertEqual("042007", concat_digits("0Foo42 bar007 test"))].

%% @private
%% @doc Counts number of spaces in a string.
%% @spec count_spaces(String) -> Integer
count_spaces(String) -> string:len([ A || A <- String, A =:= 32]).

count_spaces_test_() ->
    [?_assertEqual(0, count_spaces("Foobar")),
     ?_assertEqual(1, count_spaces("Foo bar")),
     ?_assertEqual(2, count_spaces("Foo bar test"))].
