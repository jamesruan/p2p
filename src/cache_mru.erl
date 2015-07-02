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
%% cache_mru: a gen_cache module that implement MRU replacement policy.
%%
%% @end
%% -----------------------------------------------------------------------------
-module(cache_mru).
-author('James Ruan').
-vsn({0,1,0}).

-behaviour(gen_cache).
-export([new/1]).
-export([init/1, reset/1, handle_lookup/3, handle_touch/3, handle_insert/3, handle_update/3, handle_replace/3]).

-type rw() :: gen_cache:rw().
-type args() :: #{size => integer()}.
-type state() :: term().

%% @doc
%% Args: 'size' is max size of the cache.
-spec new(
	Args :: args() | #{}
	) -> Cache :: gen_cache:cache().
new(Args) ->
	cache_ru:new(?MODULE, Args).

%% callbacks:
-spec init(Args :: #{}) -> State :: state().
init(Args) ->
	cache_ru:init(Args).

-spec reset(State :: state()) -> NState :: state().
reset(State) ->
	cache_ru:reset(State).

-spec handle_lookup(Key :: term(), RW :: rw(), State :: state()) ->
	none |
	{none, Full :: boolean()} |
	{ok, Value :: term()} |
	{ok, Value :: term(), Full :: boolean()}.
handle_lookup(Key, RW, State) ->
	cache_ru:handle_lookup(Key, RW, State).

-spec handle_touch(Key :: term(), RW :: rw(), State :: state()) -> NState :: state().
handle_touch(Key, RW, State) ->
	cache_ru:handle_touch(Key, RW, State).
	
-spec handle_insert(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
handle_insert(Key, Value, State) ->
	cache_ru:handle_insert(Key, Value, State).

-spec handle_update(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
handle_update(Key, Value, State) ->
	cache_ru:handle_update(Key, Value, State).

-spec handle_replace(Key :: term(), Value :: term(), State :: state()) -> NState :: state().
handle_replace(Key, Value, State) ->
	cache_ru:handle_replace(Key, Value, State).
