-module(master).

-export([init/1]).

-import(slave, [init/0]).

init(N) ->
    put(nodes, [spawn_monitor() || _ <- lists:seq(1, N)]),
    loop().

spawn_monitor() ->
    Pid = spawn(slave, init, []),
    monitor(process, Pid),
    Pid.

loop() ->
    receive
        {'DOWN', _, process, Object, Info} ->
            io:format("Process ~p died because ~p, another one will born ~n", [Object, Info]),
            Nodes = get(nodes),
            NewNode = spawn_monitor(),
            State = rpc(next_slave(), {get_all}),
            _ = rpc(NewNode, {update_all, State}),
            put(nodes, [NewNode | lists:delete(Object, Nodes)]),
            loop();
        {From, {store, K, V}} ->
            case lists:all(fun (X) -> X =:= ok end, [rpc(X, {store, K, V}) || X <- get(nodes)]) of
               true -> From ! {self(), ok};
               false -> From ! {self(), error}
            end,
            loop();
        {From, {lookup, K}} ->
            From ! {self(), rpc(next_slave(), {lookup, K})},
            loop();
        {From, {exit}} ->
            From ! {self(), ok},
            lists:foreach(fun (X) -> exit(X, normal) end, get(nodes)),
            exit(self(), normal)
    end.

next_slave() ->
    [One | T] = get(nodes),
    put(nodes, lists:append(T, [One])),
    One.

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

simple_store_test() ->
    Pid = spawn(fun() -> init(3) end),
    ?assertEqual(ok, rpc(Pid, {store, a, 1})).

simple_lookup_test() ->
    Pid = spawn(fun() -> init(3) end),
    _ = rpc(Pid, {store, a, 1}),
    ?assertEqual([1], rpc(Pid, {lookup, a})).

-endif.

