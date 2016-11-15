-module(counting_calls).

%% API exports
-export([start/0, service/1]).

%%====================================================================
%% API functions
%%====================================================================
start() ->
    register(supervisor, spawn(fun() -> loop() end)),
    register(slave, spawn(fun() -> loop() end)).

service(exit) ->
    slave ! exit,
    supervisor ! exit;

service(log) ->
    S = whereis(supervisor),
    S ! {self(), results},
    receive
        {S, results, Reply} ->
            Reply
    end;

service(Service) ->
    S = whereis(slave),
    S ! {self(), service, {name, Service}},
    receive
        {S, reply, Reply} ->
            Reply
    end.

%%====================================================================
%% Internal functions
%%====================================================================
loop() ->
    receive
        {From, results} ->
            From ! {self(), results, get()},
            loop();
        {log, {called, Service}} ->
            case get(Service) of
                undefined -> put(Service, 1);
                N -> put(Service, N+1)
            end,
            loop();
        {From, service, {name, getElem}} ->
            supervisor ! {log, {called, getElem}},
            From ! {self(), reply, getElem},
            loop();
        {From, service, {name, dummy}} ->
            supervisor ! {log, {called, dummy}},
            From ! {self(), reply, dummy},
            loop();
        exit ->
            exit("Shutdown gracefully"),
            ok
    end.
%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup(Fn) ->
    start(),
    Fn(),
    service(exit).

simple_getElem_test() ->
    setup(fun () -> ?assertEqual(getElem, service(getElem)) end).

simple_dummy_test() ->
    setup(fun() -> ?assertEqual(dummy, service(dummy)) end).

simple_log_test() ->
    setup(fun() ->
                  service(getElem),
                  service(getElem),
                  service(dummy),
                  ?assertEqual([{getElem, 2}, {dummy, 1}], service(log))
          end
     ).

-endif.
