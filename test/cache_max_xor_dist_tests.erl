-module(cache_max_xor_dist_tests).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
	Args = #{size => 4, base => <<0>>},
	Cache = cache_max_xor_dist:new(Args),
	?assertMatch(cache_max_xor_dist, maps:get(module, Cache)).

get_put_hit_update_reset_test() -> 
	Args = #{size => 4, base => <<0>>},
	Cache = cache_max_xor_dist:new(Args),
	{Key, Value} = {<<1>>, "1"},
	NCache = gen_cache:cache(Key, Value, Cache),
	?assertNotMatch(NCache, Cache),
	{Value, N2Cache} = gen_cache:query(Key, NCache),
	?assertMatch(NCache, N2Cache),
	N3Cache = gen_cache:cache(Key, "2", N2Cache),
	?assertNotMatch(N3Cache, N2Cache),
	N4Cache = gen_cache:flush(N3Cache),
	?assertMatch(N4Cache, Cache).
	
fulldrop_test() ->
	Args = #{size => 4, base => <<0>>},
	Cache = cache_max_xor_dist:new(Args),
	NCache  = gen_cache:cache(<<1>>, "1", Cache),
	N2Cache = gen_cache:cache(<<2>>, "2", NCache),
	N3Cache = gen_cache:cache(<<3>>, "3", N2Cache),
	N4Cache = gen_cache:cache(<<4>>, "4", N3Cache),
	N5Cache = gen_cache:cache(<<5>>, "5", N4Cache),
	?assertMatch(none, gen_cache:query(<<4>>, N5Cache)),
	?assertMatch({"5", _}, gen_cache:query(<<5>>, N5Cache)).
	
