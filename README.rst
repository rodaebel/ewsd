=================
Web Socket Server
=================

This is a lightweight Web Socket server written in Erlang implementing the
protocol described here:

  http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-76


Copyright and License
---------------------

Copyright 2011 Tobias Rodaebel

This software is released under the Apache License, Version 2.0. You may obtain
a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0


Credits
-------

Thanks to Jesse Farmer for a great article about building generalized TCP
servers which heavily influenced this software.

  http://20bits.com/articles/erlang-a-generalized-tcp-server


Requirements
------------

The Web Socket server requires Erlang/OTP R14A or higher.


Building and Running
--------------------

In order to build and run the server, enter the following commands::

  $ make
  $ bin/ewsd


Web Socket Handlers
-------------------

This library introduces a `websocket_handler` behavior which aims at providing
some convenience for developing new Web Socket handlers.

A typical Web Socket handler consists of the following methods::

  init_handler, handle_message, handle_close

Included in this distribution you find a brief example on how to implement a
simple echo handler.

The Web Socket handler module must be configered in the `websocket.app` file by
adding this tuple `{handler, ModuleName}` to the environment::

  {env, [{ip, any}, {port, 8888}, {handler, websocket_echo}, {timeout, 5000}]}

Alternatively, a configuration file can be specified. The examples directory
contains a broadcast handler. In order to run the Web Socket server with the
related configuration file, enter the following command::

  $ bin/ewsd --config=examples/broadcast.config
