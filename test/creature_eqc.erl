-module(creature_eqc).

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

initial_state() -> init.

init(_) ->
    [{error, {call, ?MODULE, stop, []}},
     {cheese_day, {call, ?MODULE, start_link, [cheese_day]}}].

cheese_day(_) ->
    [{init, {call, ?MODULE, stop, []}},
     {error, {call, ?MODULE, new_day, [cheese]}},
     {lettuce_day, {call, ?MODULE, new_day, [lettuce]}},
     {grapes_day, {call, ?MODULE, new_day, [grapes]}}].

error(_) -> [].

grapes_day(_) ->
    [{init, {call, ?MODULE, stop, []}},
     {error, {call, ?MODULE, new_day, [grapes]}},
     {lettuce_day, {call, ?MODULE, new_day, [lettuce]}},
     {cheese_day, {call, ?MODULE, new_day, [cheese]}}].

lettuce_day(_) ->
    [{init, {call, ?MODULE, stop, []}},
     {error, {call, ?MODULE, new_day, [lettuce]}},
     {grapes_day, {call, ?MODULE, new_day, [grapes]}},
     {cheese_day, {call, ?MODULE, new_day, [cheese]}}].


%% ===================================================================
%% PRECONDITION
%% ===================================================================

precondition(_From, _To, _S, {call, _, new_day, [_]}) ->
    true;
precondition(_From, _To, _S,
	     {call, _, start_link, [_]}) ->
    true;
precondition(_From, _To, _S, {call, _, stop, []}) ->
    true.

%% ===================================================================
%% MODEL
%% ===================================================================

initial_state_data() -> [].

next_state_data(_From, _To, S, _V,
		{call, _, new_day, [_]}) ->
    S;
next_state_data(_From, _To, S, _V,
		{call, _, start_link, [_]}) ->
    S;
next_state_data(_From, _To, S, _V,
		{call, _, stop, []}) ->
    S.

%% ===================================================================
%% POSTCONDITION
%% ===================================================================

postcondition(_From, error, _S, _Call, R) ->
    case R of
      {'EXIT', _} -> true;
      _ -> false
    end;
postcondition(_From, _To, _S, {call, _, new_day, [_]},
	      R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end;
postcondition(_From, _To, _S,
	      {call, _, start_link, [_]}, R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end;
postcondition(_From, _To, _S, {call, _, stop, []}, R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end.

prop_creature() ->
    ?FORALL(Cmds, (commands(?MODULE)),
	    begin
	      {History, S, Res} = run_commands(?MODULE, Cmds),
	      cleanup(S),
	      ?WHENFAIL((bluefringe_fsm:pp_eunit(creature,
						 eqc_statem:zip(Cmds,
								[R
								 || {_, R}
									<- History]))),
			(Res == ok))
	    end).

%%====================================================================
%% FSM API
%%====================================================================

new_day(X1) -> catch creature:new_day(X1).

start_link(X1) -> catch creature:start_link(X1).

stop() -> catch creature:stop().

cleanup(_S) -> catch begin creature:stop() end.

-endif. % (EQC).
-endif. % (TEST).
 
