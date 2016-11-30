-module(res).

%% API exports
-export([reverse_request/2, close/0]).

-define(SERVER_PID, {server, 'server@Air-di-Luca'}).

%%====================================================================
%% API functions
%%====================================================================

reverse_request(Str, N) ->
    ?SERVER_PID ! {{self(), node()}, reverse, Str, N},
    receive
        {?SERVER_PID, reversed, Rev} ->
            io:format("~p~n", [Rev]),
            ok;
        X ->
            X
    end.

close() ->
    ?SERVER_PID ! {self(), close},
    receive
        {?SERVER_PID, closed} ->
            io:format("The service is closed!!!"),
            ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================



%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


-endif.
