% Multi-process hello world example
-module(multiProcessHello).
-export([hello/1]).

hello(N) when is_integer(N) ->
   [
      spawn(fun() -> io:format("Hello world from process ~b~n", [I]) end) || I <- lists:seq(1,N)
   ].
