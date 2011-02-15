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
