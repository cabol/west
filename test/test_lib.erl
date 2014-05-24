%% Copyright
-module(test_lib).

%% API
-export([print_test/2,
         print_test_err/2,
         test_out/1,
         test_out_fail/1]).

print_test(Event, Args) ->
    io:format("Event: ~p~nArgs: ~p~n", [Event, Args]).

print_test_err(Event, Args) ->
    io:format("Event: ~p~nArgs: ~p~n", [Event, Args]),
    receive
        _ -> ok
    after
        2000 -> ok = err
    end.

test_out(_A) ->
    ok.

test_out_fail(_A) ->
    {401, "You are not authorized."}.
