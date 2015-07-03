%% Copyright (C) 2015 James Ruan <ruanbeihong@gmail.com>
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along
%% with this program; if not, write to the Free Software Foundation, Inc.,
%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%% -----------------------------------------------------------------------------
%% @author James Ruan <ruanbeihong@gmail.com>
%% @doc
%% gen_cache: generic cache module.
%% <pre>
%% new/2         init/1
%% query/2       handle_lookup/3, handle_touch/3
%% cache/3       handle_lookup/3, handle_insert/3, when missed
%%               handle_lookup/3, handle_replace/3, when missed and full
%%               handle_lookup/3, handle_update/3 or handle_touch/3, when hit
%% flush/1       handle_reset/1
%% </pre>
%% @end
%% -----------------------------------------------------------------------------
-module(gen_cache).
-author('James Ruan').
-vsn({0,1,0}).

-export([new/2, query/2, lookup/2, cache/3, flush/1]).
-export_type([cache/0, rw/0, state/0]).

-opaque cache() :: #{module => module(), state => state()}.

-type rw() :: read | write.
-type state() :: term().

%% initiation.
-callback init(Args :: #{}) -> State :: state().

%% clean the data but hold the configuration.
-callback reset(State :: state()) -> NState :: state().

%% try hit the cache for read, read only.
-callback handle_lookup(Key :: term(), RW :: rw(), State :: state()) ->
	none |
	{none, Full :: boolean()} |
	{ok, Value :: term()} |
	{ok, Value :: term(), Full :: boolean()}.

%% update cache when read is hit.
-callback handle_touch(Key :: term(), RW :: rw(), State :: state()) -> NState :: state().

%% write cache when write is not hit.
-callback handle_insert(Key :: term(), Value :: term(), State :: state()) -> NState :: state().

%% write cache when write is hit.
-callback handle_update(Key :: term(), Value :: term(), State :: state()) -> NState :: state().

%% replace cache line when write is hit but cache is full
-callback handle_replace(Key :: term(), Value :: term(), State :: state()) -> NState :: state().

%% @doc create a new cache initialized with `Args'.
%% @end
-spec new(
	Mod :: module(),
	Args :: #{}
	) -> Cache :: cache().
new(Mod, Args) ->
	State = Mod:init(Args),
	#{module => Mod, state => State}.

%% @doc flush a cache by clean its data while preserving its configuration.
%% @end
-spec flush(Cache :: cache()) -> NCache :: cache().
flush(Cache) ->
	Mod = maps:get(module, Cache),
	State = maps:get(state, Cache),
	Cache#{state := Mod:reset(State)}.

%% @doc find an item in cache without updating its internal structure.
%% @end
-spec lookup(
	Key :: term(),
	Cache :: cache()) -> none | (Value :: term()).
lookup(Key, Cache) ->
	Mod = maps:get(module, Cache),
	State = maps:get(state, Cache),
	case Mod:handle_lookup(Key, read, State) of
	none ->	
		none;
	{ok, Value} ->
		Value
	end.

%% @doc find an item in cache, and update its internal structure.
%% @end
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

%% @doc put an item into cache, and update its internal structure.
%% @end
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

