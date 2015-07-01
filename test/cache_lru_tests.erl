-module(cache_lru_tests).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
	Args = #{size => 4},
	Cache = cache_lru:new(Args),
	?assertMatch(Args, maps:get(args, Cache)),
	?assertMatch(cache_lru, maps:get(module, Cache)).

get_put_test() ->
	Args = #{size => 4},
	Cache = cache_lru:new(Args),
	{Key, Value} = {1, "1"},
	{ok, NCache} = gen_cache:put(Key, Value, Cache),
	?assertNotMatch(NCache, Cache),
	?assertMatch({Value, _}, gen_cache:get(Key, NCache)),
	?assertMatch(none, gen_cache:getLine(2, NCache)).

hit_update_reset_test() -> 
	Args = #{size => 4},
	Cache = cache_lru:new(Args),
	{Key, Value} = {1, "1"},
	{ok, NCache} = gen_cache:put(Key, Value, Cache),
	{_ , N2Cache} = gen_cache:get(Key, NCache),
	?assertNotMatch(N2Cache, NCache),
	N3Cache = gen_cache:empty(N2Cache),
	?assertMatch(N3Cache, Cache),
	NValue = "2",
	{ok, N4Cache} = gen_cache:putLine({Key, NValue}, NCache),
	?assertNotMatch(N4Cache, N2Cache),
	?assertMatch({{Key, NValue}, _ }, gen_cache:getLine(Key, N4Cache)).

fulldrop_test() ->
	Args = #{size => 4},
	Cache = cache_lru:new(Args),
	{ok, NCache} = gen_cache:put(1, "1", Cache),
	{ok, N2Cache} = gen_cache:put(2, "2", NCache),
	{ok, N3Cache} = gen_cache:put(3, "3", N2Cache),
	{ok, N4Cache} = gen_cache:put(4, "4", N3Cache),
	{ok, N5Cache} = gen_cache:put(5, "5", N4Cache),
	?assertMatch(none, gen_cache:getLine(1, N5Cache)),
	?assertMatch({"5", _}, gen_cache:get(5, N5Cache)).
