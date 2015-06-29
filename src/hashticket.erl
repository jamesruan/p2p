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
%% Hashticket: a hashcash v1 like proof-of-work algorithm
%%
%% @end
%% -----------------------------------------------------------------------------

-module(hashticket).
-author('James Ruan').
-vsn({0,1,0}).

-export([generate/3, generate/2, verify/1]).

-type stamp() :: bitstring().

%% @doc Generate a stamp.
%% format is:
%%
%%   ``ver:Bits:Time:Res:Offset:Rand:Count''
%%
%% where
%%
%%   - ver: "1"
%%
%%   - Bits: the Bits of MSB collisioned to be 0
%%
%%   - Time: UTC time: YYYYMMDDhhmmss
%%
%%   - Res: input Resource if it's short (less than 32 bytes) and printable, or the sha hash digest of Resource
%%
%%   - Offset: allowed differernt in seconds when verifying in the future
%%
%% @end
-spec generate(
	Resource :: iolist(),
	Bits :: integer(),
	Offset :: integer()
	) -> stamp().
generate(Resource, Bits, Offset) ->
	ResStr = case io_lib:printable_unicode_list(Resource) andalso erlang:iolist_size(Resource) < 32 of
		true ->
			Resource;
		false ->
			Hashed = crypto:hash(sha, Resource),
			base64:encode_to_string(Hashed)
		end,
	TimeStr = universaltime(),
	RandStr = base64:encode_to_string(crypto:rand_bytes(12)),
	partial_collision(ResStr, Bits, TimeStr, Offset, RandStr).

%% @doc Generate a stamp with default Offset 3600 (an hour).
%% @see generate/3
%% @end
-spec generate(
	Resource :: iolist(),
	Bits :: integer()
	) -> stamp().
generate(Resource, Bits) ->
	generate(Resource, Bits, 3600).

%% @doc Verify a stamp
-spec verify(
	Stamp :: stamp()) -> boolean().
verify(Stamp) ->
	Now = universaltime(),
	NowInt = erlang:list_to_integer(Now),
	StrStamp = erlang:bitstring_to_list(Stamp),
	["1", BitsStr, TimeStr, _Res, OffsetStr | _] = string:tokens(StrStamp, ":"),
	Bits = erlang:list_to_integer(BitsStr),
	Time = erlang:list_to_integer(TimeStr),
	Offset = erlang:list_to_integer(OffsetStr),

	Hash = crypto:hash(sha, Stamp),
	case Hash of
		<<0:Bits, _/bits>> ->
			NowInt - Time =< Offset; 
		_ ->
			false
	end.
%% @end
			
%% interal functions
-spec partial_collision(
	Res :: string(),
	Bits :: integer(),
	Time :: string(),
	Offset :: integer(),
	Rand :: string()
	) -> bitstring().

partial_collision(Res, Bits, Time, Offset, Rand) ->
	Ver = "1",
	CountStr = base64:encode_to_string(crypto:rand_bytes(12)),
	StampStr = Ver ++ ":" ++ erlang:integer_to_list(Bits) ++ ":"
		 ++ Time ++ ":" ++ Res ++ ":" ++ erlang:integer_to_list(Offset) ++":"
		 ++ Rand ++ ":" ++ CountStr,
	Stamp = erlang:list_to_bitstring(StampStr),
	case crypto:hash(sha, Stamp) of
		<<0:Bits, _/bits>> ->
			Stamp;
		_ ->
			partial_collision(Res, Bits, Time, Offset, Rand)
	end.

-spec universaltime() -> string().
universaltime() ->
	{Date, Time} = erlang:universaltime(),
	DateStr = lists:flatten(
		lists:map(fun(A) -> erlang:integer_to_list(A) end,
		          erlang:tuple_to_list(Date))),
	TimeStr = lists:flatten(
		lists:map(fun(A) -> erlang:integer_to_list(A) end,
		          erlang:tuple_to_list(Time))),
	DateStr ++ TimeStr.
