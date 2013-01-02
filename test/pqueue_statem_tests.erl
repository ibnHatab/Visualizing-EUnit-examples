%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%   Test module for priority queue: http://jlouisramblings.blogspot.fr/search?q=proper
%%% @end
%%% Created : 31 Dec 2012 by vlad <lib.aca55a@gmail.com>

-module(pqueue_statem_tests).

-ifdef(TEST).


-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(proper_statem).
-export([command/1, initial_state/0, next_state/3, postcondition/3, precondition/2]).

%% Exporting midel to ProPer for check_speck
-export([listq_ins/2,
         listq_peek/1,
         listq_ppeek/1,
         listq_prio_peek/2,
         listq_rem/1,
         listq_rem/2,
         listq_to_list/1,
         listq_len/1
        ]). 

%% ===================================================================
%% the model lightly tested by check_specs
%% erl -pa .eunit/ -pa test/ -noshell -noinput \
%%     -eval "proper:check_specs(pqueue_statem_tests, [verbose, long_result])." \
%%     -s erlang halt
%% ===================================================================

%% A listq is the sorted list of properties 
-type priority() :: integer().
-type element() :: pos_integer().

-type listq() :: [{priority(), [element()]}].

-spec listq_ins({priority(), element()}, listq()) -> listq().
listq_ins({P, V}, []) ->
    [{P, [V]}];
listq_ins({P, V}, [{P1, _} | _] = LQ) when  P < P1 ->
    [{P, [V]} | LQ];
listq_ins({P,V}, [{P1, Vs} | Next]) when P == P1 ->
    [{P, Vs ++ [V]} | Next];
listq_ins({P,V}, [{P1, Vs} | Next]) when  P > P1 ->
    [{P1, Vs} | listq_ins({P,V}, Next)].

-spec listq_to_list(listq()) -> [element()].
listq_to_list(L) ->
    lists:concat([Vs || {_P,Vs} <- L]).

-spec listq_len(listq()) -> pos_integer() | 0.
listq_len(L) ->
    lists:sum([length(Vs) || {_P,Vs} <- L]).

-spec listq_rem(listq()) -> listq().
listq_rem([])                      ->    [];
listq_rem([{_, [_V]} | Next])      ->    Next;
listq_rem([{P, [_V | Vs]} | Next]) ->    [{P,Vs} | Next];
listq_rem([{_, []} | Next])        ->    listq_rem(Next).

-spec listq_rem(listq(), priority()) -> listq().
listq_rem([], _P)                    ->    [];
listq_rem([{P, [_]} | Next], P)      -> Next;
listq_rem([{P, [_ | Vs]} | Next], P) -> [{P, Vs} | Next];
listq_rem([{P1, Vs} | Next], P)      -> [{P1, Vs} | listq_rem(Next, P)].

-spec listq_peek(listq()) -> {'value', element()} | 'empty'.
listq_peek([])                 -> 'empty';
listq_peek([{_P,[]} | Next])   -> listq_peek(Next);
listq_peek([{_P,[V | _]} | _]) -> {'value', V}.

-spec listq_ppeek(listq()) -> {'value', element(), priority()} | 'empty'.
listq_ppeek([])                -> 'empty';
listq_ppeek([{_P,[]} | Next])  -> listq_ppeek(Next);
listq_ppeek([{P,[V | _]} | _]) -> {'value', V, P}.


-spec listq_prio_peek(listq(), priority()) -> {'value', element()} | 'empty'.
listq_prio_peek([], _P)                -> 'empty';
listq_prio_peek([{P, [V | _]} | _], P) -> {'value', V};
listq_prio_peek([{_P1, _} | Next], P)  -> listq_prio_peek(Next, P).

 
%% ===================================================================
%% EUnit tests
%% ===================================================================
insert_test_() ->
    ?_assertEqual([{0,[1]}], listq_ins({0,1},[])),
    ?_assertEqual([{0,[1]}, {1,[11]}], listq_ins({1,11},
                                                 listq_ins({0,1},[]))),
    ?_assertEqual([{0,[1]}, {1,[11, 12]}], listq_ins({1,12},
                                                     listq_ins({1,11},
                                                               listq_ins({0,1},[])))).


%% eqc_test_no() ->
%%     {timeout, 30,
%%      {spawn,
%%       [
%%        {timeout, 15, ?_assertMatch(_, eqc_fsm:visualize(?MODULE))}
%%       ]
%%      }}.


%% ===================================================================
%% Statem
%% ===================================================================

-record(state,{in_queue :: listq()}).
-define(SERVER, queue_srv).

%% Generators
priority() -> integer(-20,20).
value() -> pos_integer().
    

priority_in(InQ) ->
    elements([P || {P, _} <- InQ]).

%% Initialize the state
initial_state() ->
    #state{in_queue = []}.

%% Command generator, S is the state
command(#state{in_queue = InQ}) ->
    oneof([{call, ?SERVER, in, [value()]}
           , {call, ?SERVER, in, [value(), priority()]}
           , {call, ?SERVER, is_empty, []}
           , {call, ?SERVER, is_queue, []}
           , {call, ?SERVER, len, []}
           , {call, ?SERVER, out, []}
           , {call, ?SERVER, pout, []}
           , {call, ?SERVER, to_list, []}]
          ++[{call, ?SERVER, out, [priority_in(InQ)]} || InQ =/= []]
           %% , {call, ?SERVER, , []}
           %% , {call, ?SERVER, , []}
           %% , {call, ?SERVER, , []}
         ).

%% Next state transformation, S is the current state
next_state(#state{in_queue = InQ} = S,_V,{call,_,in,[V,P]}) ->
    S#state{in_queue=listq_ins({P, V}, InQ)};
next_state(#state{in_queue = InQ} = S,_V,{call,_,in,[V]}) ->
    S#state{in_queue=listq_ins({0, V}, InQ)};

next_state(#state{in_queue = InQ} = S,_V,{call,_,out,_}) ->
    S#state{in_queue=listq_rem(InQ)};
next_state(#state{in_queue = InQ} = S,_V,{call,_,pout,_}) ->
    S#state{in_queue=listq_rem(InQ)};
next_state(#state{in_queue = InQ} = S,_V,{call,_,out,[P]}) ->
    S#state{in_queue=listq_rem(InQ, P)};

next_state(#state{in_queue = InQ} = S,_V,{call,_,_,_}) ->
    S.

%% Precondition, checked before command is added to the command sequence
precondition(_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)

%% Model head should match removed element
postcondition(#state{in_queue = InQ} = S,{call,_,out,[P]},Res) ->
    Res == listq_prio_peek(InQ,P);
postcondition(_S,{call,_,_,_},_Res) ->
    true.

prop_listq_correct() ->
    numtests(300,
             ?FORALL(Cmds,commands(?MODULE),
                     ?TRAPEXIT(
                        begin
                            %% io:format(">> ~p~n", [Cmds]),
                            ?SERVER:start_link(pqueue),
                            {H,S,Res} = run_commands(?MODULE,Cmds),
                            ?SERVER:stop(),
                            ?WHENFAIL(
                               begin
                                   io:format("Comands:\n"),
                                   lists:foreach(fun (C) -> io:format("    ~p~n", [C]) end, Cmds),
                                   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res])
                               end,
                               Res == ok)
                        end)
                    )
            ).


-endif. % (TEST).
