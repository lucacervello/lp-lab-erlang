-module(eval).

%% API exports
-export([parse/1, eval/1]).

%%====================================================================
%% API functions
%%====================================================================
eval({minus, Exp1, Exp2}) ->
    eval(Exp1) - eval(Exp2);
eval({plus, Exp1, Exp2}) ->
    eval(Exp1) + eval(Exp2);
eval({times, Exp1, Exp2}) ->
    eval(Exp1) * eval(Exp2);
eval({num, X}) ->
    X.

% completamente con parentesi quindi é semplice (N Op N)
parse([$(, N1, Op, N2, $)]) ->
    case Op of
        $- -> {minus, parse(N1), parse(N2)};
        $+ -> {plus, parse(N1), parse(N2)};
        $* -> {times, parse(N1), parse(N2)}
    end;

parse(X) when X >= $0, X =< $9 ->
    {num, X}.

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

num_eval_test() ->
    ?assertEqual(4, eval({num, 4})).

simple_eval_test() ->
    ?assertEqual(4, eval({plus, {num, 2}, {num, 2}})).

simple_parse_test() ->
    ?assertEqual({minus, {num, 4}, {num, 3}}, "(4-3)").

-endif.
