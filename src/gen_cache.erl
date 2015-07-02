-module(gen_cache).

-export([new/2, query/2, cache/3, flush/1]).
-export_type([cache/0]).

-opaque cache() :: #{module => module(), state => term()}.

-type rw() :: read | write.

%% @doc
%% new/2         init/1
%% query/2       handle_lookup/3, handle_touch/3
%% cache/3       handle_lookup/3, handle_insert/3, when missed
%%               handle_lookup/3, handle_replace/3, when missed and full
%%               handle_lookup/3, handle_update/3 or handle_touch/3, when hit
%% flush/1       handle_reset/1
%% @end

%% @doc initiation
%% @end
-callback init(Args :: #{}) -> State :: term().

%% @doc clean the data but hold the configuration
%% @end
-callback reset(State :: term()) -> NState :: term().

%% @doc try hit the cache for read, read only
%% @end
-callback handle_lookup(Key :: term(), RW :: rw(), State :: term()) ->
	none |
	{none, Full :: boolean()} |
	{ok, Value :: term()} |
	{ok, Value :: term(), Full :: boolean()}.

%% @doc update cache when read is hit
%% @end
-callback handle_touch(Key :: term(), RW :: rw(), State :: term()) -> NState :: term().

%% @doc write cache when write is not hit
%% @end
-callback handle_insert(Key :: term(), Value :: term(), State :: term()) -> NState :: term().

%% @doc write cache when write is hit
%% @end
-callback handle_update(Key :: term(), Value :: term(), State :: term()) -> NState :: term().

%% @doc replace cache line when write is hit but cache is full
%% @end
-callback handle_replace(Key :: term(), Value :: term(), State :: term()) -> NState :: term().

-spec new(
	Mod :: module(),
	Args :: #{}
	) -> Cache :: cache().
new(Mod, Args) ->
	State = Mod:init(Args),
	#{module => Mod, state => State}.

-spec flush(Cache :: cache()) -> NCache :: cache().
flush(Cache) ->
	Mod = maps:get(module, Cache),
	State = maps:get(state, Cache),
	Cache#{state := Mod:reset(State)}.

-spec query(
	Key :: term(),
	Cache :: cache()) -> none | {Value :: term(), NCache :: cache()}.
query(Key, Cache) ->
	Mod = maps:get(module, Cache),
	State = maps:get(state, Cache),
	case Mod:handle_lookup(Key, read, State) of
	none ->	
		none;
	{ok, Value} ->
		NState = Mod:handle_touch(Key, read, State),
		{Value, Cache#{state := NState}}
	end.

-spec cache(
	Key :: term(),
	Value :: term(),
	Cache :: cache()) -> NCache :: cache().
cache(Key, Value, Cache) ->
	Mod = maps:get(module, Cache),
	State = maps:get(state, Cache),
	NState = case Mod:handle_lookup(Key, write, State) of
	{none, true} ->
		Mod:handle_replace(Key, Value, State);
	{none, false} ->
		Mod:handle_insert(Key, Value, State);
	{ok, Value, _} ->
		Mod:handle_touch(Key, write, State);
	{ok, _OValue, _} ->
		Mod:handle_update(Key, Value, State)
	end,
	Cache#{state := NState}.

