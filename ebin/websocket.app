%% -*-Erlang-*-
{application, websocket,
 [{description, "Web Socket Server"},
  {vsn, "1.0.0"},
  {modules, [websocket, websocket_app, websocket_sup, websocket_lib,
             websocket_server, websocket_handler, websocket_echo]},
  {registered, [websocket_sup, websocket_server]},
  {applications, [kernel, stdlib]},
  {mod, {websocket_app, []}},
  {env, [{ip, any}, {port, 8888}, {handler, websocket_echo}, {timeout, 10000}]}
 ]}.
%% vim: set filetype=erlang :
