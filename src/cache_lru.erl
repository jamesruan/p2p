-module(cache_lru).
-ifndef(CACHE_LRU_DEFAULT_SIZE).
-define(CACHE_LRU_DEFAULT_SIZE, 128).
-endif.
-behaviour(gen_cache).
-export([new/1]).
-export([init/1, reset/1, handle_lookup/3, handle_touch/3, handle_insert/3, handle_update/3, handle_replace/3]).

-type rw() :: read | write.
-type args() :: #{size => integer()}.
-type data() :: #{kt => gb_trees:tree(), tv => gb_trees:tree()}.
-type state() :: #{args => args(), data => data()}.

%% @doc
%% Args: 'size' is max size of the cache.
-spec new(
	Args :: args() | #{}
	) -> Cache :: gen_cache:cache().
new(Args) ->
	gen_cache:new(?MODULE, Args).

%% callbacks:
-spec init(Args :: #{}) -> State :: term().
init(Args) ->
	Size = maps:get(size, Args, ?CACHE_LRU_DEFAULT_SIZE),	
	NArgs = Args#{size => Size},
	#{args => NArgs, data => #{
		%% {Key, {Time, Value} => {Key, Time}, {Time, Value} 
		kt => gb_trees:empty(),
		tkv => gb_trees:empty()}}.

-spec reset(State :: term()) -> NState :: state().
reset(State) ->
	State#{data := #{
		kt => gb_trees:empty(),
		tkv => gb_trees:empty()}}.

-spec handle_lookup(Key :: term(), RW :: rw(), State :: term()) ->
	none |
	{none, Full :: boolean()} |
	{ok, Value :: term()} |
	{ok, Value :: term(), Full :: boolean()}.
handle_lookup(Key, RW, State) ->
	Args = maps:get(args, State),
	Size = maps:get(size, Args),
	Data = maps:get(data, State),
	TKV = maps:get(tkv, Data),
	KT = maps:get(kt, Data),
	case RW of
	read ->
		case gb_trees:lookup(Key, KT) of
		none ->
			none;
		{value, Time} ->
			{Key, Value} = gb_trees:get(Time, TKV),
			{ok, Value}
		end;
	write ->
		case gb_trees:lookup(Key, KT) of
		none ->
			{none, gb_trees:size(KT) == Size};
		{value, Time} ->
			{Key, Value} = gb_trees:get(Time, TKV),
			{ok, Value, gb_trees:size(KT) == Size}
		end
	end.

-spec handle_touch(Key :: term(), RW :: rw(), State :: term()) -> NState :: term().
handle_touch(Key, _RW, State) ->
	Data = maps:get(data, State),
	KT = maps:get(kt, Data),
	TKV = maps:get(tkv, Data),
	Now = erlang:now(),
	Time = gb_trees:get(Key, KT),
	{Key, Value} = gb_trees:get(Time, TKV),
	NData = Data#{ kt := gb_trees:update(Key, Now, KT),
	               tkv := gb_trees:insert(Now, {Key, Value}, gb_trees:delete(Time, TKV))},
	State#{data := NData}.
	
-spec handle_insert(Key :: term(), Value :: term(), State :: term()) -> NState :: term().
handle_insert(Key, Value, State) ->
	Data = maps:get(data, State),
	KT = maps:get(kt, Data),
	TKV = maps:get(tkv, Data),
	Now = erlang:now(),
	NData = Data#{ kt := gb_trees:insert(Key, Now, KT),
	               tkv := gb_trees:insert(Now, {Key, Value}, TKV)},
	State#{data := NData}.

-spec handle_update(Key :: term(), Value :: term(), State :: term()) -> NState :: term().
handle_update(Key, Value, State) ->
	Data = maps:get(data, State),
	KT = maps:get(kt, Data),
	TKV = maps:get(tkv, Data),
	Now = erlang:now(),
	Time = gb_trees:get(Key, KT),
	NData = Data#{ kt := gb_trees:update(Key, Now, KT),
	               tkv := gb_trees:insert(Now, {Key, Value}, gb_trees:delete(Time, TKV))},
	State#{data := NData}.

-spec handle_replace(Key :: term(), Value :: term(), State :: term()) -> NState :: term().
handle_replace(Key, Value, State) ->
	Data = maps:get(data, State),
	KT = maps:get(kt, Data),
	TKV = maps:get(tkv, Data),
	Now = erlang:now(),
	{_Time, {OKey, _OValue}, NTKV} = gb_trees:take_smallest(TKV),
	NKT = gb_trees:delete(OKey, KT),
	NData = Data#{ kt := gb_trees:insert(Key, Now, NKT),
	               tkv := gb_trees:insert(Now, {Key, Value}, NTKV)},
	State#{data := NData}.

