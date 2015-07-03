-module(cache_mru_tests).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
	Args = #{size => 4},
	Cache = cache_mru:new(Args),
	?assertMatch(cache_mru, maps:get(module, Cache)).

get_put_test() ->
	Args = #{size => 4},
	Cache = cache_mru:new(Args),
	{Key, Value} = {1, "1"},
	NCache = gen_cache:cache(Key, Value, Cache),
	?assertNotMatch(NCache, Cache),
	{Value, N2Cache} = gen_cache:query(Key, NCache),
	?assertNotMatch(N2Cache, NCache),
	Value = gen_cache:lookup(Key, NCache),
	?assertMatch(none, gen_cache:query(2, NCache)),
	?assertMatch(none, gen_cache:lookup(2, NCache)).

hit_update_reset_test() -> 
	Args = #{size => 4},
	Cache = cache_mru:new(Args),
	{Key, Value} = {1, "1"},
	NCache = gen_cache:cache(Key, Value, Cache),
	{Value, N2Cache} = gen_cache:query(Key, NCache),
	?assertNotMatch(N2Cache, NCache),
	N3Cache = gen_cache:cache(Key, Value, N2Cache),
	N4Cache = gen_cache:cache(Key, "2", N3Cache),
	N5Cache = gen_cache:flush(N4Cache),
	?assertMatch(N5Cache, Cache).

fulldrop_test() ->
	Args = #{size => 4},
	Cache = cache_mru:new(Args),
	NCache  = gen_cache:cache(1, "1", Cache),
	N2Cache = gen_cache:cache(2, "2", NCache),
	N3Cache = gen_cache:cache(3, "3", N2Cache),
	N4Cache = gen_cache:cache(4, "4", N3Cache),
	N5Cache = gen_cache:cache(5, "5", N4Cache),
	?assertMatch(none, gen_cache:query(4, N5Cache)),
	?assertMatch({"5", _}, gen_cache:query(5, N5Cache)).
