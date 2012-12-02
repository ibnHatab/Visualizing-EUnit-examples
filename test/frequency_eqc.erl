%%% File    : frequency_eqc.erl
%%% Description : EQC FSM for frequency module

-module(frequency_eqc).

-ifdef(TEST).
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_fsm.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


%% ===================================================================
%% EUnit tests
%% ===================================================================
eqc_test_() ->
    {timeout, 30,
     {spawn, 
      [
       {timeout, 15, ?_assertMatch(_, eqc_fsm:visualize(?MODULE))}
      ]
     }}.

%% ===================================================================
%% FSM
%% ===================================================================

-define(MAX_FREQ, 3).

-record(freq,{used=[], free=[]}).

initial_state() -> state_init.

state_init(_) ->
    [
     {{allocated, 0}, {call, ?MODULE, start, [?MAX_FREQ]}},
     {state_error, {call, ?MODULE, allocate, []}},
     {state_error, {call, ?MODULE, deallocate, [nat()]}},
     {state_error, {call, ?MODULE, stop, []}}
    ].

allocated(N,S) ->
	[{state_error, {call, ?MODULE, start, [nat()]}}] ++
	[{state_error, {call, ?MODULE, deallocate, [elements(S#freq.free)]}}
	  || N == 0] ++
	[{{allocated, N+1}, {call, ?MODULE, allocate, []}}
	 || N < ?MAX_FREQ] ++
	[{state_error, {call, ?MODULE, allocate, []}}
	 || N == ?MAX_FREQ] ++
	[{{allocated, N-1}, {call, ?MODULE, deallocate, [elements(S#freq.used)]}}
	 || N > 0] ++
	[{state_init, {call, ?MODULE, stop, []}}].
	
state_error(_) -> [].

%% ===================================================================
%% PRECONDITION
%% ===================================================================

precondition(_From, _To, _S, {call, _, allocate, []}) ->
    true;
precondition(_From, _To, _S, {call, _, deallocate, [_]}) ->
    true;
precondition(_From, _To, _S, {call, _, start, [_]}) ->
    true;
precondition(_From, _To, _S, {call, _, stop, []}) ->
    true.

%% ===================================================================
%% MODEL
%% ===================================================================

initial_state_data() -> #freq{}.

next_state_data(_From, _To, S, _V, {call, _, start, [Max]}) ->
    S#freq{used=[],
	   free=lists:seq(1, Max)};
next_state_data(_From, _To, S, V, {call, _, allocate, []}) ->
    case S#freq.free == [] of
	true -> S;
	false ->
	    S#freq{used=S#freq.used++[V],
		   free=S#freq.free--[V]}
    end;
next_state_data(_From, _To, S, _V, {call, _, deallocate, [Freq]}) ->
    S#freq{used=S#freq.used--[Freq],
	   free=S#freq.free++[Freq]};
next_state_data(_From, _To, S, _V,
		{call, _, stop, []}) ->
    S#freq{used=[], free=[]}.

%% ===================================================================
%% POSTCONDITION
%% ===================================================================

postcondition(state_init, allocated, _S, {call, _, start, []}, R) ->
    is_pid(R);
postcondition(allocated, state_init, _S, {call, _, stop, []}, R) ->
    R = ok;
postcondition(_From, _To, S, {call, _, allocate, []}, R) ->
    case R of
	{error, no_frequency} -> S#freq.free == [];
	F when is_integer(F) ->
	    lists:member(F, S#freq.free)
    end;
postcondition(_From, To, _S, {call, _, deallocate, [_Freq]}, R)
  when To /= state_error->
    R = ok;
postcondition(_From, state_error, _S, _Call, R) ->
    case R of
	{'EXIT', _} -> true;
	_ -> false
    end;
postcondition(_From, _To, _S, {call, _, start, [_]}, R) ->
    case R of
	{'EXIT', _} -> false;
	_ -> true
    end;
postcondition(_From, _To, _S, {call, _, stop, []}, R) ->
    case R of
	{'EXIT', _} -> false;
	_ -> true
    end.

prop_frequency() ->
    ?FORALL(Cmds, (commands(?MODULE)),
	    begin
		{History, S, Res} = run_commands(?MODULE, Cmds),
		cleanup(S),
		?WHENFAIL((bluefringe_fsm:pp_eunit(frequency,
						   eqc_statem:zip(Cmds,
								  [R
								   || {_, R}
									  <- History]))),
			  (Res == ok))
	    end).

%%====================================================================
%% FSM API
%%====================================================================

allocate() ->
    case frequency:allocate() of
    	{ok, Freq} -> Freq;
    	Error -> Error
    end.

deallocate(X1) ->
    frequency:deallocate(X1).

start(Freqs) ->
    catch frequency:start(lists:range(1, Freqs)).

stop() -> catch frequency:stop().

cleanup(_S) -> catch begin stop() end.


-endif. % (EQC).
-endif. % (TEST).
