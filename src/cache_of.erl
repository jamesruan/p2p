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
%% cache_of: a gen_cache module that implement an one-factor min/max replacement policy.
%%
%% @end
%% -----------------------------------------------------------------------------
-module(cache_of).
-author('James Ruan').
-vsn({0,1,0}).

-ifndef(CACHE_OF_DEFAULT_SIZE).
-define(CACHE_OF_DEFAULT_SIZE, 128).
-endif.

-behaviour(gen_cache).
-export([new/2]).
-export([init/1, reset/1, handle_lookup/3, handle_touch/3, handle_insert/3, handle_update/3, handle_replace/3]).
-export_type([droptype/0, factor_fun/0]).

-type rw() :: gen_cache:rw().
-type cache() :: gen_cache:cache().
-type droptype() :: min | max.
%% `min' for replace with min(Factor).
%%
%% `max' for replace with max(Factor).
-type factor() :: term().
-type factor_fun() :: fun(({OFactor :: factor(), {Key:: term(), Value :: term()}}, FArgs :: term()) -> factor()).
%% `OFactor' old Factor.
%%
%% `Key' `Value' current Key and Value.
%%
%% `FArgs' argument for calculate a new Factor.

-type args() :: #{
	size => integer(),
	factor_args => term(),
	new_factor => factor_fun(),
	drop => droptype() }.
-type data() :: #{kfv => gb_trees:tree(), fkv => gb_trees:tree()}.
-type state() :: #{args => args(), data => data()}.

%% @doc create a new cache
-spec new(
	Mod :: module(),
	Args :: args() | #{}
	) -> Cache :: cache().
new(Mod, Args) ->
	gen_cache:new(Mod, Args).

%% callbacks:
-spec init(Args :: #{}) -> State :: state().
init(Args) ->
	Size = maps:get(size, Args, ?CACHE_OF_DEFAULT_SIZE),
	NArgs = Args#{size => Size},
	#{args => NArgs, data => #{
		%% {Key, Value} => {Key, {Factor, Value}}, {{Factor, Key}, Value}
		kfv => gb_trees:empty(),
		fkv => gb_trees:empty()}}.

-spec reset(State :: state()) -> NState :: state().
reset(State) ->
	State#{data := #{
		kfv => gb_trees:empty(),
		fkv => gb_trees:empty()}}.

-spec handle_lookup(Key :: term(), RW :: rw(), State :: state()) ->
	none |
	{none, Full :: boolean()} |
	{ok, Value :: term()} |
	{ok, Value :: term(), Full :: boolean()}.
handle_lookup(Key, RW, State) ->
	Args = maps:get(args, State),
	Size = maps:get(size, Args),
	Data = maps:get(data, State),
	KFV = maps:get(kfv, Data),
	case RW of
	read ->
		case gb_trees:lookup(Key, KFV) of
		none ->
			none;
		{value, {_Factor, Value}} ->
			{ok, Value}
		end;
	write ->
		case gb_trees:lookup(Key, KFV) of
		none ->
			{none, gb_trees:size(KFV) == Size};
		{value, {_Factor, Value}} ->
			{ok, Value, gb_trees:size(KFV) == Size}
		end
	end.

-spec handle_touch(Key :: term(), RW :: rw(), State :: state()) -> NState :: state().
handle_touch(Key, _RW, State) ->
	Data = maps:get(data, State),
	KFV = maps:get(kfv, Data),
	FKV = maps:get(fkv, Data),

	Args = maps:get(args, State),
	New_factor = maps:get(new_factor, Args),
	FactorArgs = maps:get(factor_args, Args),
	{OFactor, OValue} = gb_trees:get(Key, KFV),
	Factor = New_factor({OFactor, {Key, OValue}}, FactorArgs),

	NData = Data#{ kfv := gb_trees:update(Key, {Factor, OValue}, KFV),
	               fkv := gb_trees:insert({Factor, Key}, OValue, gb_trees:delete({OFactor, Key}, FKV))},
	State#{data := NData}.
	
-spec handle_insert(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
handle_insert(Key, Value, State) ->
	Data = maps:get(data, State),
	KFV = maps:get(kfv, Data),
	FKV = maps:get(fkv, Data),

	Args = maps:get(args, State),
	New_factor = maps:get(new_factor, Args),
	FactorArgs = maps:get(factor_args, Args),
	OFactor = none,
	Factor = New_factor({OFactor, {Key, Value}}, FactorArgs),

	NData = Data#{ kfv := gb_trees:insert(Key, {Factor, Value}, KFV),
	               fkv := gb_trees:insert({Factor, Key}, Value, FKV)},
	State#{data := NData}.

-spec handle_update(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
handle_update(Key, Value, State) ->
	Data = maps:get(data, State),
	KFV = maps:get(kfv, Data),
	FKV = maps:get(fkv, Data),

	Args = maps:get(args, State),
	New_factor = maps:get(new_factor, Args),
	FactorArgs = maps:get(factor_args, Args),
	{OFactor, _OValue} = gb_trees:get(Key, KFV),
	Factor = New_factor({OFactor, {Key, Value}}, FactorArgs),

	NData = Data#{ kfv := gb_trees:update(Key, {Factor, Value}, KFV),
	               fkv := gb_trees:insert({Factor, Key}, Value, gb_trees:delete({OFactor, Key}, FKV))},
	State#{data := NData}.

-spec handle_replace(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
handle_replace(Key, Value, State) ->
	Data = maps:get(data, State),
	KFV = maps:get(kfv, Data),
	FKV = maps:get(fkv, Data),

	Args = maps:get(args, State),
	New_factor = maps:get(new_factor, Args),
	FactorArgs = maps:get(factor_args, Args),
	Drop = maps:get(drop, Args),

	{{OFactor, OKey}, _OValue, NFKV} = case Drop of
	min -> 
		gb_trees:take_smallest(FKV);
	max ->
		gb_trees:take_largest(FKV)
	end,
	Factor = New_factor({OFactor, {Key, Value}}, FactorArgs),

	NData = Data#{ kfv := gb_trees:insert(Key, {Factor, Value}, gb_trees:delete(OKey, KFV)),
	               fkv := gb_trees:insert({Factor, Key}, Value, NFKV)},
	State#{data := NData}.

