-module(echo).

%% API exports
-export([start/0, print/1, stop/0]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
    register(server, spawn(fun() -> loop() end)),
    ok.

print(M) ->
    server ! {print, M},
    ok.


stop() ->
    server ! exit,
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

loop() ->
    receive
        {print, M} ->
            io:format("Message: ~p~n", [M]),
            loop();
        exit ->
            exit("Shutdown gracefully")
    end.


%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% I don't know how to test against io:format and i don't want to find out

stupid_start_test() ->
    ?assertEqual(ok, start()),
    stop().

stupid_print_test() ->
    start(),
    ?assertEqual(ok, print("stupid")),
    stop().

stupid_stop_test() ->
    start(),
    ?assertEqual(ok, stop()).

-endif.
