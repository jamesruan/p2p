-module(gen_cache).

-export([new/2, empty/1, getLine/2, get/2, putLine/2, put/3]).

-type cache() :: #{module => module()}.

-callback init(
	Args :: #{}
	) -> Cache :: cache().

-callback reset(Cache :: cache()) -> NCache ::cache().

-callback get_value_by_key(
	Key :: term(),
	Cache :: cache()
	) -> none | { Value :: term(), NCache :: cache() }.

-callback put_value_by_key(
	Key :: term(),
	Value :: term(),
	Cache :: cache()
	) -> { ok, NCache :: cache() }.

-spec new(
	Mod :: module(),
	Args :: #{}
	) -> Cache :: cache().
new(Mod, Args) ->
	Cache = Mod:init(Args),
	Cache#{module => Mod}.

-spec empty(Cache :: cache()) -> NCache :: cache().
empty(Cache) ->
	Mod = maps:get(module, Cache),
	Mod:reset(Cache).

-spec getLine(
	Key :: term(),
	Cache :: cache()
	) ->
	none |
	{ {Key :: term(), Value :: term()}, NCache :: cache()}.
getLine(Key, Cache) ->
	case gen_cache:get(Key, Cache) of
	none ->
		none;
	{Value, NCache} ->
		{ {Key, Value}, NCache}
	end.

-spec get(
	Key :: term(),
	Cache :: cache()
	) ->
	none |
	{ Value :: term(), NCache :: cache()}.
get(Key, Cache) ->
	Mod = maps:get(module, Cache),
	Mod:get_value_by_key(Key, Cache).
	

-spec putLine(
	{Key :: term(), Value :: term()},
	Cache :: term()
	) -> {ok, NCache :: cache()}.
putLine( {Key, Value}, Cache) ->
	gen_cache:put(Key, Value, Cache).

-spec put(
	Key :: term(),
	Value :: term(),
	Cache :: term()
	) -> {ok, NCache :: cache()}.
put(Key, Value, Cache) ->
	Mod = maps:get(module, Cache),
	Mod:put_value_by_key(Key, Value, Cache).
