% This is an -*- erlang -*- file.

{application, erlzmq,
 [{description, "ZeroMQ"},
  {vsn, "0.1"},
  {modules, [zmq]},
  {registered,[]},
  {applications, [kernel,stdlib]},
  {env, []}
 ]
}.
