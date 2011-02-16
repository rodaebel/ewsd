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
    {ok, {Headers, Path, Body}} = parse_request(Bin),

    Origin = proplists:get_value(origin, Headers),
    Host = proplists:get_value(host, Headers),
    Key1 = proplists:get_value('sec-websocket-key1', Headers),
    Key2 = proplists:get_value('sec-websocket-key2', Headers),

    Challenge = calc_challenge(Key1, Key2, list_to_binary(Body)),

    % Web Socket Handshake response
    Response =
    "HTTP/1.1 101 WebSocket Protocol Handshake\r\n"
    "Upgrade: WebSocket\r\nConnection: Upgrade\r\n"
    "Sec-WebSocket-Origin: " ++ Origin ++ "\r\n"
    "Sec-WebSocket-Location: ws://" ++ Host ++ Path ++ "\r\n"
    "Sec-WebSocket-Protocol: chat\r\n\r\n" ++ Challenge,

    {ok, Response, Path}.

%% Pivate API

%% @private
%% @doc Parses handshake request.
%% @spec parse_request(Bin) -> {ok, Headers, Body}
parse_request(Bin) ->
    {ok, {http_request, _, P, _}, _} = erlang:decode_packet(http_bin, Bin, []),
    {abs_path, Path} = P,
    L = binary_to_list(Bin),
    [H|[Body]] = re:split(L, "\r\n\r\n", [{return, list}]),
    Head = re:split(H, "\r\n", [{return, list}]),
    {Key1, Key2, Origin, Host} = parse_header(lists:reverse(Head)),
    Headers = [{origin, Origin},
               {host, Host},
               {'sec-websocket-key1', Key1},
               {'sec-websocket-key2', Key2}],
    {ok, {Headers, binary_to_list(Path), Body}}.

parse_request_test_() ->
    [?_assertEqual(
        {ok, {[{origin,"http://localhost"},
               {host,"localhost:8888"},
               {'sec-websocket-key1',"2P  0?8)62 372 ' 0 0"},
               {'sec-websocket-key2',"2I0 6y66?  \"5710 hW ^%#)n"}],
              "/",
              [221,202,132,253,200,62,10,237]}},
        parse_request(
<<71,69,84,32,47,32,72,84,84,80,47,49,46,49,13,10,85,112,103,114,97,100,101,58,
  32,87,101,98,83,111,99,107,101,116,13,10,67,111,110,110,101,99,116,105,111,
  110,58,32,85,112,103,114,97,100,101,13,10,72,111,115,116,58,32,108,111,99,97,
  108,104,111,115,116,58,56,56,56,56,13,10,79,114,105,103,105,110,58,32,104,
  116,116,112,58,47,47,108,111,99,97,108,104,111,115,116,13,10,83,101,99,45,87,
  101,98,83,111,99,107,101,116,45,75,101,121,49,58,32,50,80,32,32,48,63,56,41,
  54,50,32,51,55,50,32,39,32,48,32,48,13,10,83,101,99,45,87,101,98,83,111,99,
  107,101,116,45,75,101,121,50,58,32,50,73,48,32,54,121,54,54,63,32,32,34,53,
  55,49,48,32,104,87,32,94,37,35,41,110,13,10,13,10,221,202,132,253,200,62,10,
  237>>)),
     ?_assertEqual(
        {ok, {[{origin,"http://localhost"},
               {host,"localhost:8888"},
               {'sec-websocket-key1',"Z4Df3t+Rx89  C4338{[s"},
               {'sec-websocket-key2',"1 I 897266A82 5"}],
              "/",
              [190,21,91,4,144,224,69,137]}},
        parse_request(
<<71,69,84,32,47,32,72,84,84,80,47,49,46,49,13,10,85,112,103,114,97,100,101,58,
  32,87,101,98,83,111,99,107,101,116,13,10,67,111,110,110,101,99,116,105,111,
  110,58,32,85,112,103,114,97,100,101,13,10,72,111,115,116,58,32,108,111,99,97,
  108,104,111,115,116,58,56,56,56,56,13,10,79,114,105,103,105,110,58,32,104,
  116,116,112,58,47,47,108,111,99,97,108,104,111,115,116,13,10,83,101,99,45,87,
  101,98,83,111,99,107,101,116,45,75,101,121,49,58,32,90,52,68,102,51,116,43,
  82,120,56,57,32,32,67,52,51,51,56,123,91,115,13,10,83,101,99,45,87,101,98,83,
  111,99,107,101,116,45,75,101,121,50,58,32,49,32,73,32,56,57,55,50,54,54,65,
  56,50,32,53,13,10,13,10,190,21,91,4,144,224,69,137>>))].

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
