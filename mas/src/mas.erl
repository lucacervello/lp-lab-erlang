-module(mas).

%% API exports
-export([start/1, to_slave/2, stop/0]).

%%====================================================================
%% API functions
%%====================================================================
start(N) ->
    register(master, spawn(fun()->init(N) end)).

to_slave(Message, N) ->
    rpc(whereis(master), {message, Message, slave, N}).

stop() ->
    master ! exit,
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

init(N) ->
    [put(X, spawn_monitor()) || X <- lists:seq(1, N)],
    master_loop().

spawn_monitor() ->
    Pid = spawn(fun()->slave_loop() end),
    monitor(process, Pid),
    Pid.

master_loop() ->
    receive
        {'DOWN', _, process, Object, _} ->
            [{K, _} | _] = lists:filter(fun({_, V}) -> Object =:= V end, get()),
            put(K, spawn_monitor()),
            master_loop();
        {From, {message, M, slave, N}} ->
            From ! {self(), rpc(get(N), {message, M})},
            master_loop();
        exit ->
            lists:foreach(fun ({_, X}) -> exit(X, normal) end, get()),
            unregister(master),
            exit(self(), normal)
    end.

slave_loop() ->
    receive
        {From, {message, M}} ->
            From ! {self(), M}
    end.

rpc(From, Q) ->
    From ! {self(), Q},
    receive
        {From, Reply} ->
            Reply
    end.

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    start(20),
    ?assertEqual(ok, stop()).

simple_slave_test() ->
    start(20),
    ?assertEqual("ciao", to_slave("ciao", 4)),
    stop().



-endif.
