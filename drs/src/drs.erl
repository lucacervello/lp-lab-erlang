-module(drs).

%% API exports
-export([reverse_string/1, reverse_string/2]).

-define(STRING_LENGTH, 100).

%%====================================================================
%% API functions
%%====================================================================

reverse_string(Str) ->
    reverse_string(Str, ?STRING_LENGTH).

reverse_string(Str, StringLenght) ->
    NProcess = length(Str) div StringLenght,
    register(master, spawn(fun () -> init(NProcess + 1) end)),
    R = rpc(whereis(master), {reverse_string, Str}),
    _ = rpc(whereis(master), stop),
    R.

%%====================================================================
%% Internal functions
%%====================================================================
rpc(From, Q) ->
    From ! {self(), Q},
    receive
        {From, Reply} ->
            Reply
    end.

spawn_monitor() ->
    Pid = spawn(fun () -> slave_loop() end),
    monitor(process, Pid),
    Pid.

init(N) ->
    _ = [put(X, spawn_monitor()) || X <- lists:seq(1, N)],
    master_loop().

master_loop() ->
    receive
        {From, {reverse_string, Str}} ->
            StrList = split_string(Str, (length(Str) div length(get()))+1),
            StrReversed = [rpc(X, {reverse_string, S}) || {{_, X}, S} <- lists:zip(get(), StrList)],
            From ! {self(), lists:foldl(fun(X, Acc) -> X ++ Acc end, [], StrReversed)},
            master_loop();
        {From, stop} ->
            lists:foreach(fun ({_, X}) -> exit(X, normal) end, get()),
            From ! {self(), ok},
            exit(self(), normal)
    end.


slave_loop() ->
    receive
        {From, {reverse_string, Str}} ->
            From ! {self(), lists:reverse(Str)}
    end.

split_string(Str, N) when N > length(Str) ->
    [Str];
split_string(Str, N) ->
    {S1, S2} = lists:split(N, Str),
    [S1 | split_string(S2, N)].

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

split_string_test() ->
    ?assertEqual(["ciao", "ciao", []], split_string("ciaociao", 4)).

simple_test() ->
    ?assertEqual("ossoossoossoosso", reverse_string("ossoossoossoosso", 4)).

long_test() ->
    Reversed = ".ecivres gnirts_desrever_gnol eht ot tupni na sa dessap M cireneg a ot net morf sgnirtsbus fo rebmun eht no tniartsnoc eht xaler ot yrt esicrexe eht htiw enod nehW.gnirts nevig a no ecivres eht rof ksa tsuj )ssecorPtneilC( ssecorp tneilc eht.stluser 01 eht snioj dna ))(gnirts_esrever ecivres( ⁹w ,... ,⁰w sgnirtsbus 01 eht esrever ot )ssecorPevalS( srotca tcnitsid 01 ot sdrawrof dna 01/w#=⁹w#=...=¹⁺ⁿw# e 1+01/w#=ⁿw#=...=⁰w# :)01%w#=n dna ,gnirts a fo htgnel rotarepo eht stneserper #( shtgnel gniwollof eht htiw ⁹w ,... ,⁰w sgnirts 01 otni w tupni eht sesopmoced )(gnirts_desrever_gnol ecivres eht;gnirts desrever eht snruter )sretcarahc 0001 ≥( gnirts egral yrev a nevig taht )(gnirts_esrever_gnol dellac ecivres a sedivorp )ssecorPretsaM( ssecorp revres a:erehw metsys detubirtsid )esicrexe suoiverp eht ees( evals-retsam a etirW.ssecorp eht tsaf ot aedi doog a si tupni trohs a no syawla gnikrow mhtirogla eht peek ot redro ni sgnirts detrevni eht kcab nioj ot dna sgnirts retrohs ni gnirts tupni eht esopmoced oT .wols ylemertxe snaem dluoc laivirt sworg gnirts eht fo htgnel eht nehw tub laivirt etiuq si gnirts a ni sretcarahc eht fo redro eht esrever oT",
    Original = "To reverse the order of the characters in a string is quite trivial but when the length of the string grows trivial could means extremely slow. To decompose the input string in shorter strings and to join back the inverted strings in order to keep the algorithm working always on a short input is a good idea to fast the process.Write a master-slave (see the previous exercise) distributed system where:a server process (MasterProcess) provides a service called long_reverse_string() that given a very large string (≥ 1000 characters) returns the reversed string;the service long_reversed_string() decomposes the input w into 10 strings w⁰, ..., w⁹ with the following lengths (# represents the operator length of a string, and n=#w%10): #w⁰=...=#wⁿ=#w/10+1 e #wⁿ⁺¹=...=#w⁹=#w/10 and forwards to 10 distinct actors (SlaveProcess) to reverse the 10 substrings w⁰, ..., w⁹ (service reverse_string()) and joins the 10 results.the client process (ClientProcess) just ask for the service on a given string.When done with the exercise try to relax the constraint on the number of substrings from ten to a generic M passed as an input to the long_reversed_string service.",
    ?assertEqual(Reversed, reverse_string(Original, 100)).



-endif.
