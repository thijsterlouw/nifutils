-module(nifutils_tests).
-include_lib("eunit/include/eunit.hrl").


now_test_() ->
	{A,_,_} = erlang:now(),
    [
	 	?_assertMatch({A,_,_} , nifutils:nif_now())
	].


time_test_() ->
	{Mega, Sec, _} = erlang:now(),
	Time = (Mega * 1000000) + Sec,
    [
	 	?_assert(( Time - nifutils:nif_time() ) =< 1)
	].


random_test_() ->
	Random = nifutils:nif_random(),
	[
	 	?_assert(is_integer(Random))
	].

uniform_test_() ->
	Random4 = nifutils:nif_uniform(4),
	Random2 = nifutils:nif_uniform(2),
	[
	 	?_assert(Random4 >= 1 andalso Random4 =< 4),
	 	?_assert(Random2 >= 1 andalso Random2 =< 2),
		?_assertException(error,badarg, nifutils:nif_uniform(1))
	].

rot13_test_() ->
	[
	 	?_assertEqual("nnnooonnn", nifutils:nif_rot13("aaabbbaaa")),
    	?_assertEqual("nopqrstuvwxyzabcdefghijklm",  nifutils:nif_rot13("abcdefghijklmnopqrstuvwxyz")),
		?_assertEqual("123no", nifutils:nif_rot13("123ab"))
	].

