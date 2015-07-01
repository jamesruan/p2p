-module(cache_lru).
-ifndef(CACHE_LRU_DEFAULT_SIZE).
-define(CACHE_LRU_DEFAULT_SIZE, 128).
-endif.
-behaviour(gen_cache).
-export([new/1]).
-export([init/1, reset/1, get_value_by_key/2, put_value_by_key/3]).

-type args() :: #{size => integer()}.
-type state() :: #{kt => gb_trees:tree(), tv => gb_trees:tree()}.
-type cache() :: #{args => args(), state => state()}.

%% @doc
%% Args: 'size' is max size of the cache.
-spec new(
	Args :: args() | #{}
	) -> Cache :: cache().
new(Args) ->
	gen_cache:new(?MODULE, Args).

%% callbacks:
-spec init(
	Args :: #{}
	) -> Cache :: cache().

init(Args) ->
	Size = maps:get(size, Args, ?CACHE_LRU_DEFAULT_SIZE),	
	NArgs = Args#{size => Size},
	#{args => NArgs, state => #{
		%% {Key, {Time, Value} => {Key, Time}, {Time, Value} 
		kt => gb_trees:empty(),
		tkv => gb_trees:empty()}}.

-spec reset(Cache :: cache()) -> NCache ::cache().
reset(Cache) ->
	Cache#{state := #{
		kt => gb_trees:empty(),
		tkv => gb_trees:empty()}}.
	
-spec get_value_by_key(
	Key :: term(),
	Cache :: cache()
	) -> none | { Value :: term(), NCache :: cache() }.
get_value_by_key( Key, Cache ) ->
	State = maps:get(state, Cache),
	KT = maps:get(kt, State),
	TKV = maps:get(tkv, State),
	case gb_trees:lookup(Key, KT) of
	none ->
		none;
	{value, Time} ->
		{Key, Value} = gb_trees:get(Time, TKV),
		% update time
		Now = erlang:now(),
		NState = State#{ kt := gb_trees:update(Key, Now, KT),
			             tkv := gb_trees:insert(Now, {Key, Value}, gb_trees:delete(Time, TKV)) },
		{ Value, Cache#{ state := NState } }
	end.

-spec put_value_by_key(
	Key :: term(),
	Value :: term(),
	Cache :: cache()
	) -> { ok, NCache :: cache() }.
put_value_by_key(Key, Value, Cache) ->
	Args = maps:get(args, Cache),
	Size = maps:get(size, Args),
	State = maps:get(state, Cache),
	KT = maps:get(kt, State),
	TKV = maps:get(tkv, State),
	NState = case gb_trees:lookup(Key, KT) of
	{value, Time} ->
		Now = erlang:now(),
		State#{ kt := gb_trees:update(Key, Now, KT),
			    tkv := gb_trees:insert(Now, {Key, Value}, gb_trees:delete(Time, TKV)) };
	none ->
		case gb_trees:size(KT) < Size of
		false ->
			%% cache is full, drop earlest one:
			Now = erlang:now(),
			{_Time, {OKey, _OValue}, NTKV} = gb_trees:take_smallest(TKV),
			State#{ kt := gb_trees:insert(Key, Now, gb_trees:delete(OKey, KT)),
				    tkv := gb_trees:insert(Now, {Key, Value}, NTKV)};
		true ->
			Now = erlang:now(),
			State#{ kt := gb_trees:insert(Key, Now, KT),
			        tkv := gb_trees:insert(Now, {Key, Value}, TKV) }
		end
	end,
	{ ok, Cache#{ state := NState } }.
