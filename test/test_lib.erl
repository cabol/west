%% Copyright
-module(test_lib).

%% API
-export([print_test/2, print_test_err/2]).

print_test(Event, Args) ->
    io:format("Event: ~p~nArgs: ~p~n", [Event, Args]).

print_test_err(Event, Args) ->
    io:format("Event: ~p~nArgs: ~p~n", [Event, Args]),
    receive
        _ -> ok
    after
        2000 -> ok = err
    end.
