%% -*-Erlang-*-
{application, websocket,
 [{description, "Web Socket Server"},
  {vsn, "1.0.0"},
  {modules, [websocket_app, websocket_sup, websocket_lib, websocket_server,
             websocket_handler, websocket_broadcast]},
  {registered, [websocket_sup, websocket_server]},
  {applications, [kernel, stdlib]},
  {mod, {websocket_app, []}},
  {env, [{ip, any}, {port, 8888}, {handler_module, websocket_broadcast}]}
 ]}.
%% vim: set filetype=erlang :
