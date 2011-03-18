%%% Copyright (C) 2008 Willem de Jong
%%%
%%% This is json_parser.
%%%
%%% json_parser is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% json_parser is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with json_parser.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: w.a.de.jong@gmail.com

% @doc `json_parser' is a parser for json documents. 
%
% The parser has a SAX-like API: as soon as it has processed a meaningful part
% of the JSON document (such as a '{' that starts an object, or the name of a
% name/value pair), the parser will call a handler function that then has to
% process this information.  
%
% json_parser can work in a streaming mode: when it reaches the end of the
% input data before reaching the end of the JSON document, it will call a
% 'continuation-function' to request for more data.
% 
% json_parser only works on utf-8 encoded input.
%
% As an example of how the SAX API can be used, a handler function is provided
% that produces 'mochijson2'-output.

% @reference See www.json.org for an introduction to JSON.
%
% @reference See 
% <a href="http://www.saxproject.org/apidoc/org/xml/sax/ContentHandler.html">the
% SAX API description</a> for more information SAX parsers for XML.
%
% @author Willem de Jong <w.a.de.jong@gmail.com>
% @copyright 2008 Willem de Jong
% @version 0.1

%% @type sax_event() = (startDocument | endDocument | startObject | endObject | 
%%                      startArray | endArray | sax_value_event())
%% @type sax_value_event() = {value, (integer() | float() | 
%%                          binary() | sax_atom_value())}
%% @type sax_atom_value() = null | true | false
-module(json_parser).

-export([event_stream_parser/3]).
-export([event_stream_parser/4]).
-export([dvm_parser/1]).
-export([test/0]).
-export([test_dvm/0]).
-export([test_file/1]).

-record(json_state, 
  {user_state, 
   callback,
   continuation_state,
   stack,
   continuation_fun}).

-define(is_digit(C), 
        C > 47, C < 58).
-define(is_hex_digit(C), 
        C > 47, C < 58; C > 64, C < 71).
-define(is_number_char(C), 
        C =:= $E; C =:= $e; C =:= $-; C =:= $+; C =:= $.).
-define(is_delimiter(C), 
        C =:= $]; C =:= $}; C =:= $,).
-define(is_whitespace(C), 
        C =:= 32; C =:= 13 ; C =:= 10; C =:= 9).

% @equiv event_stream_parser(Json, UserState, Handler, [])
event_stream_parser(Json, UserState, Handler) ->
  event_stream_parser(Json, UserState, Handler, []).

% @spec event_stream_parser(Json, Acc0, Handler, Options) -> {ok, Acc1, Rest}
% where Json = binary(),
%       Acc0  = term(),
%       Handler = (Event::sax_event(), AccIn::term()) -> AccOut,
%       AccIn = term(),
%       AccOut = term(),
%       Options = [Option],
%       Acc1 = term(),
%       Rest = binary(),
%       Option = {continuation_function, CFunction, C_Acc0},
%       CFunction = (C_StateIn::term()) -> C_StateOut
%       C_StateOut = term()
% @doc Parses a JSON document.
%
% event_stream_parser(Json, Acc0, Handler, Options) calls Handler(Event, AccIn)
% on successive "JSON SAX events" that result from parsing Json, starting with
% AccIn == Acc0. Handler/2 must return a new accumulator which is passed to the
% next call. The function returns {ok, AccOut, Tail}, where AccOut is the final
% value of the accumulator and Tail the remaining bytes that follow after the
% end of the Json document.
%
% (Notice how similar this is to lists:foldl(Fun, Acc0, Json_sax_events),
% assuming that Json_sax_events is the list of Sax events - I more or less
% copied this description from the documentation of the lists module.)
%
% CFunction should be a function that takes 1 arguments: State. It should
% return the next block of data, and a new value for State. This new value will
% be passed to the next invocation of CFunction; it can be used to keep track
% of the location in a file or something similar.
event_stream_parser(Json, Acc0, Handler, Options) when is_binary(Json) ->
  {CFun, CState} = getCFunction(Options),
  parse(Json, 
        #json_state{callback = Handler, 
                    stack = [], 
                    user_state = Acc0,
                    continuation_state = CState,
                    continuation_fun = CFun}).

parse(<<>>, State) ->
  getMoreData(<<>>, State, fun parse/2, []);
%% ignore byte order mark
parse(<<16#EF, 16#BB, 16#BF, Doc/binary>>, State) ->
  parse(Doc, State);
parse(Doc, State) ->
  State2 = wrapCallback(startDocument, State),
  {Tail, State3} = parseValue(Doc, State2),
  State4 = wrapCallback(endDocument, State3),
  {ok, State4#json_state.user_state, Tail}.

parseValue(<<NextChar, Tail/binary>>, State) 
  when ?is_whitespace(NextChar) ->
  parseValue(Tail, State);
parseValue(<<$", Tail/binary>>, State) -> %"
  parseStringBinary(Tail, State, <<>>);
parseValue(<<${, Tail/binary>>, #json_state{stack = S} = State) ->
  parsePair(Tail, wrapCallback(startObject, State#json_state{stack =[${ | S]}));
parseValue(<<$[, Tail/binary>>, #json_state{stack = S} = State) ->
  parseValue(Tail, wrapCallback(startArray, State#json_state{stack =[$[ | S]}));
parseValue(<<"t">>, State) ->
  getMoreData(<<"t">>, State, fun parseValue/2, []);
parseValue(<<"tr">>, State) ->
  getMoreData(<<"tr">>, State, fun parseValue/2, []);
parseValue(<<"tru">>, State) ->
  getMoreData(<<"tru">>, State, fun parseValue/2, []);
parseValue(<<"true", Tail/binary>>, State) ->
  parseToDelimiter(Tail, reportValue(true, State));
parseValue(<<>>, State) ->
  getMoreData(<<>>, State, fun parseValue/2, []);
parseValue(<<"f">>, State) ->
  getMoreData(<<"f">>, State, fun parseValue/2, []);
parseValue(<<"fa">>, State) ->
  getMoreData(<<"fa">>, State, fun parseValue/2, []);
parseValue(<<"fal">>, State) ->
  getMoreData(<<"fal">>, State, fun parseValue/2, []);
parseValue(<<"fals">>, State) ->
  getMoreData(<<"fals">>, State, fun parseValue/2, []);
parseValue(<<"false", Tail/binary>>, State) ->
  parseToDelimiter(Tail, reportValue(false, State));
parseValue(<<"n">>, State) ->
  getMoreData(<<"n">>, State, fun parseValue/2, []);
parseValue(<<"nu">>, State) ->
  getMoreData(<<"nu">>, State, fun parseValue/2, []);
parseValue(<<"nul">>, State) ->
  getMoreData(<<"nul">>, State, fun parseValue/2, []);
parseValue(<<"null", Tail/binary>>, State) ->
  parseToDelimiter(Tail, reportValue(null, State));
parseValue(<<N, Tail/binary>>, State) when ?is_digit(N); N =:= $- ->
  parseNumber(Tail, State, [N], integer);
parseValue(<<$], Tail/binary>>, #json_state{stack = [$[ | S] } = State) -> 
  parseToDelimiter(Tail, wrapCallback(endArray, State#json_state{stack = S}));
parseValue(<<_/binary>>, _State) ->
  throw({error, "Malformed value"}).

parsePair(<<NextChar, Tail/binary>>, State) 
  when ?is_whitespace(NextChar) ->
  parsePair(Tail, State);
parsePair(<<$", Tail/binary>>, State) -> %"
  parseKeyColonValue(Tail, State, <<>>);
parsePair(<<>>, State) ->
  getMoreData(<<>>, State, fun parsePair/2, []);
parsePair(<<$}, Tail/binary>>, #json_state{stack = [${ | S] } = State) -> 
  parseToDelimiter(Tail, wrapCallback(endObject, State#json_state{stack = S})).

parseStringBinary(<<$", Tail/binary>>, State, StringSoFar) -> %"
  parseToDelimiter(Tail, reportValue(StringSoFar, State));
parseStringBinary(<<"\\">>, State, StringSoFar) -> %"
  getMoreData(<<"\\">>, State, fun parseStringBinary/3, [StringSoFar]); %"
parseStringBinary(<<$\\, $u, Tail/binary>>, State, StringSoFar) ->
  parseCodePoint(Tail, State, StringSoFar);
parseStringBinary(<<$\\, Esc, Tail/binary>>, State, StringSoFar) ->
  EscapedChar = mapEscapedChar(Esc),
  parseStringBinary(Tail, State, <<StringSoFar/binary, EscapedChar>>); 
parseStringBinary(<<Char, Tail/binary>>, State, StringSoFar) ->
%% note: there is no test on illegal or incomplete utf-8 encoding
  parseStringBinary(Tail, State, <<StringSoFar/binary, Char>>);
parseStringBinary(<<>>, State, StringSoFar) ->
  getMoreData(<<>>, State, fun parseStringBinary/3, [StringSoFar]).

parseKeyColonValue(<<$", Tail/binary>>, State, StringSoFar) -> %"
  parseColonValue(Tail, wrapCallback({key, StringSoFar}, State));
parseKeyColonValue(<<$\\>>, State, StringSoFar) ->
  getMoreData(<<$\\>>, State, fun parseKeyColonValue/3, [StringSoFar]);
parseKeyColonValue(<<$\\, $u, Tail/binary>>, State, StringSoFar) ->
  parseCodePoint(Tail, State, StringSoFar);
parseKeyColonValue(<<$\\, Esc, Tail/binary>>, State, StringSoFar) ->
  EscapedChar = mapEscapedChar(Esc),
  parseKeyColonValue(Tail, State, <<StringSoFar/binary, EscapedChar>>); 
parseKeyColonValue(<<Char, Tail/binary>>, State, StringSoFar) ->
%% note: there is no test on illegal or incomplete utf-8 encoding
  parseKeyColonValue(Tail, State, <<StringSoFar/binary, Char>>);
parseKeyColonValue(<<>>, State, StringSoFar) ->
  getMoreData(<<>>, State, fun parseKeyColonValue/3, [StringSoFar]).

parseColonValue(<<NextChar, Tail/binary>>, State) 
  when ?is_whitespace(NextChar) ->
  parseColonValue(Tail, State);
parseColonValue(<<$:, Tail/binary>>, State) ->
  parseValue(Tail, State);
parseColonValue(<<>>, State) ->
  getMoreData(<<>>, State, fun parseColonValue/2, []).

parseNumber(<<$, ,Tail/binary>>, 
                  #json_state{stack = [${ | _] } = State, Acc, Type) -> 
  parsePair(Tail, reportNumber(Acc, Type, State));
parseNumber(<<$, ,Tail/binary>>, State, Acc, Type) -> 
  parseValue(Tail, reportNumber(Acc, Type, State));
parseNumber(<<$}, Tail/binary>>, 
                  #json_state{stack = [${ | S] } = State,
                  Acc, Type) -> 
  State2 = reportNumber(Acc, Type, State),
  parseToDelimiter(Tail, wrapCallback(endObject, State2#json_state{stack = S}));
parseNumber(<<$], Tail/binary>>, 
                  #json_state{stack = [$[ | S] } = State,
                  Acc, Type) -> 
  State2 = reportNumber(Acc, Type, State),
  parseToDelimiter(Tail, wrapCallback(endArray, State2#json_state{stack = S}));
parseNumber(<<NextChar, Tail/binary>>, State, Acc, Type) when
  ?is_whitespace(NextChar) ->
  parseToDelimiter(Tail, reportNumber(Acc, Type, State));
parseNumber(<<NextChar, Tail/binary>>, State, Acc, Type) when
  ?is_digit(NextChar) ->
  parseNumber(Tail, State, [NextChar | Acc], Type);
parseNumber(<<NextChar, Tail/binary>>, State, Acc, _Type) when
  ?is_number_char(NextChar) ->
  parseNumber(Tail, State, [NextChar | Acc], float);
parseNumber(<<>>, State, Acc, Type) ->
  getMoreData(<<>>, State, fun parseNumber/4, [Acc, Type]).

reportNumber(Number, Type, State) -> 
  reportValue(case Type of 
                 integer -> list_to_integer(lists:reverse(Number));
                 _ -> list_to_float(lists:reverse(Number))
              end, State).

reportValue(Value, State) ->
  wrapCallback({value, Value}, State).

parseToDelimiter(<<Tail/binary>>, #json_state{stack = []} = State) -> 
  {Tail, State};
parseToDelimiter(<<NextChar, Tail/binary>>, State) 
  when ?is_whitespace(NextChar) ->
  parseToDelimiter(Tail, State);
parseToDelimiter(<<$, , Tail/binary>>, 
                 #json_state{stack = [${ | _S] } = State) -> 
  parsePair(Tail, State);
parseToDelimiter(<<$, , Tail/binary>>, 
                 #json_state{stack = [$[ | _S] } = State) -> 
  parseValue(Tail, State);
parseToDelimiter(<<$}, Tail/binary>>, 
                 #json_state{stack = [${ | S] } = State) -> 
  parseToDelimiter(Tail, wrapCallback(endObject, State#json_state{stack = S}));
parseToDelimiter(<<$], Tail/binary>>, 
                 #json_state{stack = [$[ | S] } = State) -> 
  parseToDelimiter(Tail, wrapCallback(endArray, State#json_state{stack = S}));
parseToDelimiter(<<>>, State) ->
  getMoreData(<<>>, State, fun parseToDelimiter/2, []).

parseCodePoint(<<Tail/binary>>, State, StringSoFar) ->
  parseCodePoint(Tail, State, StringSoFar, 3, 0).

parseCodePoint(<<Digit, Tail/binary>>, State, StringSoFar, N, Acc) when
  ?is_hex_digit(Digit) ->
  case N of
    0 -> 
      CodePoint = list_to_binary(xmerl_ucs:to_utf8(Acc + hexToDec(Digit))),
      parseStringBinary(Tail, State, <<StringSoFar/binary, CodePoint/binary>>); 
    _ -> 
      parseCodePoint(Tail, State, StringSoFar, N-1, 
                     Acc + pow(16,N)*hexToDec(Digit))
  end;
parseCodePoint(<<>>, State, StringSoFar, N, Acc) ->
  getMoreData(<<>>, State, fun parseCodePoint/5, [StringSoFar, N, Acc]).

hexToDec(N) when ?is_digit(N) -> N - 48;
hexToDec(N) -> N - 55.
pow(_, 0) -> 1;
pow(X, N) -> X * pow(X, N-1).
  
mapEscapedChar($") -> $"; %"
mapEscapedChar($\\) -> $\\;
mapEscapedChar($/) -> $/;
mapEscapedChar($b) -> 8;
mapEscapedChar($f) -> 12;
mapEscapedChar($r) -> 13;
mapEscapedChar($n) -> 10;
mapEscapedChar($t) -> 9;
mapEscapedChar(_) -> throw({error, "Malformed: Illegal escaped character"}).

wrapCallback(Event, #json_state{callback = Callback, user_state = UserState} = State) ->
  State#json_state{user_state = Callback(Event, UserState)}.

getCFunction(Options) ->
  case lists:keysearch(continuation_function,1,Options) of
    {value, {_, F, S}} -> {F, S};
    false -> {fun(T, S) -> {T, S} end, undefined}
  end.

getMoreData(Tail, State, ParseFun, Args) ->
  {Data, ContinuationState2} = 
    (State#json_state.continuation_fun)(State#json_state.continuation_state),
  case Data of 
    <<>> -> throw({error, "Malformed: Unexpected end of data"});
    _ -> 
      apply(ParseFun, 
        [<<Tail/binary, Data/binary>>, 
         State#json_state{continuation_state = ContinuationState2} |Args])
  end.

%% ----- anything below this line is intended for testing or as example ------

% @hidden A very simple test.
test() ->
  Doc = doc(),
  {ok, R, _} = event_stream_parser(list_to_binary(Doc), [], 
                             fun(Event, State) -> [Event | State] end),
  lists:reverse(R).

% @spec dvm_parser(Json::binary()) -> term()
%
% @doc An example that shows how json_event_parser() can be used to create
% normal 'document value model'-type of output.
% 
% The output created by this function is equal (or very similar) to the output
% produced by mochijson2.erl. The function can easily be modified to produce a
% different output format.
dvm_parser(Doc) -> event_stream_parser(Doc, ok, fun dvm/2).

dvm(startDocument, _) ->
  start;
dvm(startObject, Stack) ->
  [[]| Stack];
dvm(startArray, Stack) ->
  [[]| Stack];
dvm({key, _} = Event, Stack) ->
  [Event|Stack];
dvm({value, Value}, start) ->
  {value, Value};
dvm({value, Value}, [{key, Key}, List | T]) ->
  [[{Key, Value} | List] | T];
dvm({value, Value}, [List | T]) ->
  [[Value | List] | T];
dvm(endObject, [List | T]) ->
  dvm({value, {struct, lists:reverse(List)}}, T);
dvm(endArray, [List | T]) ->
  dvm({value, lists:reverse(List)}, T);
dvm(endDocument, {value, R}) ->
  R.

% @hidden a simple test for the dvm function
test_dvm() ->
  Doc = list_to_binary(doc()),
  dvm_parser(Doc).

% @doc a simple example of the use of a continuation function.
test_file(File) ->
  F = fun(Event, State) -> io:format("event: ~p~n", [Event]), State end,
  G = fun continue_file/1, %% the callback function that returns the next 
                           %% chunk of data
  {ok, Handle} = file:open(File, [read, raw, binary]),
  Position = 0,
  Chunk = 7, %% the number of bytes read - should be tuned, but 7 is
             %% definitely far too small - this is just for testing.
  CState = {Handle, Position, Chunk}, 
  SaxCallbackState = undefined,
  {ok, _Result, _TrailingBytes} = 
    event_stream_parser(<<>>, SaxCallbackState, F, 
      [{continuation_function, G, CState}]),
  ok = file:close(Handle).

%% this is a continuation function that reads chunks of data 
%% from a file.
continue_file({Handle, Offset, Chunk}) ->
  %% read the next chunk
  Data = case file:pread(Handle, Offset, Chunk) of
           {ok, Bytes} -> Bytes;
           eof -> <<>>
         end,
  {Data, {Handle, Offset + Chunk, Chunk}}.
  
%% some test data
doc() ->
  "{\"menu\" : {"
  "\"id\" : \"file\"," 
  "\"number\": -123.123E3,"
  "\"value\" : {}," 
  "\"popup\" : {"
  "\"menuitem\" : [false, true, null, \"codepoint (A): \\u0041B\", \"aap\", 13, \"newline: \\n linefeed: \\r tab: \\t quote: \\\"\", "
   "{\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"},"
   "{\"value\": \"Open\", \"onclick\": 123},"
   "{\"value\": \"Close\", \"onclick\": true}]"
  "}}}".
