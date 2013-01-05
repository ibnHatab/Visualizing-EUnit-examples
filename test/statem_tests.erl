%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%% Statem tests
%%% @end
%%% Created : 23 Dec 2012 by vlad <lib.aca55a@gmail.com>
-module(statem_tests).

-ifdef(TEST).

-define(LIC, true).

-ifdef(LIC).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-else.
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-endif.

-compile(export_all).

%% -------------------------------------------------------------------------------
%% Statem
%% --------------------------------------------------------------------------------

-record(state,{pids=[], %% pids spawned in this test
               regs=[]  %% list of {Name, Pid} pairs
              }).

pid(S) ->
    elements(S#state.pids).


name() ->
    ?LET(X, non_empty(list(range($A, $Z))), list_to_atom(X)).


dummy_proc() -> receive _ -> ok end.


spawn() ->
    erlang:spawn(?MODULE, dummy_proc, []).

register(Name, Pid) ->
    erlang:register(Name, Pid).

unregister(Name) ->
    erlang:unregister(Name).

%% Initialize the state
initial_state() ->
    #state{}.

name(S) ->
    elements(proplists:get_keys(S#state.regs) ++ [noname, novalue]).
        
%% Command generator, S is the state
command(S) ->
    InRegs = [P || {_, P} <- S#state.regs],
    FreePids = lists:subtract(S#state.pids, InRegs),
    oneof([  {call, ?MODULE, spawn, []}]
          ++ [{call, ?MODULE, register, [name(), elements(FreePids)]}
              || FreePids =/= []]
          ++ [{call, ?MODULE, unregister, [name(S)]}
              || S#state.regs =/= []]
          ++ [{call, erlang, whereis, [name(S)]}
              || S#state.regs =/= []]
         ).

%% Next state transformation, S is the current state
next_state(S,V,{call,_,spawn,_}) ->
    S#state{pids=[V|S#state.pids]};
next_state(S,_V,{call,_,register,[Name,Pid]}) ->
    S#state{regs=[{Name,Pid} | S#state.regs]};
next_state(S,_V,{call,_,unregister,[Name]}) ->
    S#state{regs=proplists:delete(Name,S#state.regs)};
next_state(S,_V,{call,_,_,_}) ->
    S.

is_registered(S, Name) ->
    %% lists:keymember(Name,1,S#state.regs).
    proplists:is_defined(Name, S#state.regs).

%% Precofndition, checked before command is added to the command sequence
precondition(S,{call,_,unregister,[Name]}) ->
    is_registered(S, Name);
precondition(S,{call,_,register,[Name, Pid]}) ->
    not is_registered(S, Name);
precondition(_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
postcondition(S,{call,_,unregister, [Name]},Res) ->
    case Res of
        {'EXIT', _} -> not is_registered(S,Name);
        true -> is_registered(S,Name)
    end;
postcondition(S,{call,_,whereis,[Name]},Res) ->
    case Res of
        undefined -> not is_registered(S,Name);
        P when is_pid(P) -> is_registered(S,Name);
        _ -> false
    end;
postcondition(_S,{call,_,spawn,[]},_Res) ->
    true;
postcondition(_S,{call,_,_,_},_Res) ->
    true.

summarize(Cmds) -> 
    lists:usort(command_names(Cmds)).

prop_register() ->
    numtests(1000,
             ?FORALL(Cmds,commands(?MODULE),
                     aggregate(summarize(Cmds),
                     begin
                         {H,S,Res} = run_commands(?MODULE,Cmds),
                         [?MODULE:unregister(N) || {N,_} <- S#state.regs],
                         [exit(P, kill) || P <- S#state.pids],
                         ?WHENFAIL(
                            begin
                                io:format("History:\n"),
                                lists:foreach(fun (C) -> io:format(" ~p~n", [C]) end, H),
                                io:format("Comands:\n"),
                                lists:foreach(fun (C) -> io:format(" ~p~n", [C]) end, Cmds),
                                io:format("State: ~p\nRes: ~p\n",[S,Res])
                            end,
                            Res == ok)
                     end
                    ))
            ).

-endif. % (TEST).



