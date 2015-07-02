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
%% cache_lru: a gen_cache module that implement LRU replacement policy.
%%
%% @end
%% -----------------------------------------------------------------------------
-module(cache_lru).
-author('James Ruan').
-vsn({0,1,0}).

-ifndef(CACHE_LRU_DEFAULT_SIZE).
-define(CACHE_LRU_DEFAULT_SIZE, 128).
-endif.

-behaviour(gen_cache).
-export([new/1]).
-export([init/1, reset/1, handle_lookup/3, handle_touch/3, handle_insert/3, handle_update/3, handle_replace/3]).

-type rw() :: gen_cache:rw().
-type args() :: #{size => integer()}.
-type data() :: #{kt => gb_trees:tree(), tkv => gb_trees:tree()}.
-type state() :: #{args => args(), data => data()}.

%% @doc
%% Args: 'size' is max size of the cache.
-spec new(
	Args :: args() | #{}
	) -> Cache :: gen_cache:cache().
new(Args) ->
	gen_cache:new(?MODULE, Args).

%% callbacks:
-spec init(Args :: #{}) -> State :: state().
init(Args) ->
	Size = maps:get(size, Args, ?CACHE_LRU_DEFAULT_SIZE),	
	NArgs = Args#{size => Size},
	#{args => NArgs, data => #{
		%% {Key, {Time, Value} => {Key, Time}, {Time, Value} 
		kt => gb_trees:empty(),
		tkv => gb_trees:empty()}}.

-spec reset(State :: state()) -> NState :: state().
reset(State) ->
	State#{data := #{
		kt => gb_trees:empty(),
		tkv => gb_trees:empty()}}.

-spec handle_lookup(Key :: term(), RW :: rw(), State :: state()) ->
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

-spec handle_touch(Key :: term(), RW :: rw(), State :: state()) -> NState :: state().
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
	
-spec handle_insert(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
handle_insert(Key, Value, State) ->
	Data = maps:get(data, State),
	KT = maps:get(kt, Data),
	TKV = maps:get(tkv, Data),
	Now = erlang:now(),
	NData = Data#{ kt := gb_trees:insert(Key, Now, KT),
	               tkv := gb_trees:insert(Now, {Key, Value}, TKV)},
	State#{data := NData}.

-spec handle_update(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
handle_update(Key, Value, State) ->
	Data = maps:get(data, State),
	KT = maps:get(kt, Data),
	TKV = maps:get(tkv, Data),
	Now = erlang:now(),
	Time = gb_trees:get(Key, KT),
	NData = Data#{ kt := gb_trees:update(Key, Now, KT),
	               tkv := gb_trees:insert(Now, {Key, Value}, gb_trees:delete(Time, TKV))},
	State#{data := NData}.

-spec handle_replace(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
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

