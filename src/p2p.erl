-module(p2p).

%% p2p: p2p library's entry point.

-export([my_func/0]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API

my_func() ->
    ok().

%% Internals

ok() ->
    ok.

%% End of Module.
-ifdef(TEST).
the_test() ->
	ok = ok.
-endif.
