-module(hashticket_tests).
-include_lib("eunit/include/eunit.hrl").

short_text_test() ->
	S2 = hashticket:generate("test", 16),
	?assert(hashticket:verify(S2)).

long_text_test() ->
	S2 = hashticket:generate("if the resource iolist is longer than 32 byte,
		 a hashed digest will be used", 16),
	?assert(hashticket:verify(S2)).

unprintable_text_test() ->
	S2 = hashticket:generate(<<1,2,3,4>>, 16),
	?assert(hashticket:verify(S2)).

forgery_test() ->
	S2 = hashticket:generate("test", 16),
	?assertNot(hashticket:verify(<<S2/bitstring, <<"f">>/bitstring>>)).

timeout_test() ->
	S3 = hashticket:generate("test", 16, 10),
	?assert(hashticket:verify(S3)),

	S3F = hashticket:generate("test", 16, 1),
	timer:sleep(2000),
	?assertNot(hashticket:verify(S3F)).
