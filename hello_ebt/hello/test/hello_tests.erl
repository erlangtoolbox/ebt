-module(hello_tests).

-include_lib("eunit/include/eunit.hrl").

hello_test() ->
    ?assertEqual(ok, hello:hello()).
