-module(processing_ring).

%% API exports
-export([start/3]).

%%====================================================================
%% API functions
%%====================================================================

start(M, N, Message) ->
    Processes = lists:map(fun (_) -> spawn(fun() -> init() end) end, lists:seq(1, N)),
    register(head, lists:nth(1, Processes)),
    ring(Processes),
    head ! {{message, Message}, {number, M}}.

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    receive
        {start, Next} ->
            loop(Next)
    end.

ring([]) ->
    error("Invalid length ring");
ring([H|[]]) ->
    H ! {start, whereis(head)};
ring([F, S |T]) ->
    F ! {start, S},
    ring([S|T]).

loop(Next) ->
    receive
        {{message, _}, {number, 0}} ->
            io:format("I'm gonna kill ~p ~n", [self()]),
            self() ! exit,
            loop(Next);
        {{message, Message}, {number, N}} ->
            io:format("Pid : ~p, number: ~p, message : ~p ~n", [self(), N, Message]),
            Next ! {{message, Message}, {number, N-1}},
            loop(Next);
        exit ->
            io:format("Shutdown gracefully ~p~n", [self()]),
            Next ! exit,
            exit("End gracefully")
    end.

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

% I can't test this module, because nothing meaningful is returned.
simple_test() ->
    ?assert(true).


-endif.
