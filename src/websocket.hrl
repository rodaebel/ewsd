-record(server_state, {port, loop, ip=any, socket=null}).

-record(protocol_state, {socket, handshake=false}).
