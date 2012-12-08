-module(locker_eqc).

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
     {unlocked, {call, ?MODULE, start, "*"}}].

unlocked(_) ->
    [{init, {call, ?MODULE, stop, []}},
     {error, {call, ?MODULE, unlock, "*"}},
     {error, {call, ?MODULE, start, "*"}},
     {locked, {call, ?MODULE, lock, "*"}}].

error(_) -> [].

locked(_) ->
    [{init, {call, ?MODULE, stop, []}},
     {error, {call, ?MODULE, start, "*"}},
     {unlocked, {call, ?MODULE, unlock, "*"}}].

%% ===================================================================
%% PRECONDITION
%% ===================================================================

precondition(_From, _To, _S, {call, _, lock, [_]}) ->
    true;
precondition(_From, _To, _S, {call, _, start, [_]}) ->
    true;
precondition(_From, _To, _S, {call, _, stop, []}) ->
    true;
precondition(_From, _To, _S, {call, _, unlock, [_]}) ->
    true.

%% ===================================================================
%% MODEL
%% ===================================================================

initial_state_data() -> [].

next_state_data(_From, _To, S, _V,
		{call, _, lock, [_]}) ->
    S;
next_state_data(_From, _To, S, _V,
		{call, _, start, [_]}) ->
    S;
next_state_data(_From, _To, S, _V,
		{call, _, stop, []}) ->
    S;
next_state_data(_From, _To, S, _V,
		{call, _, unlock, [_]}) ->
    S.


%% ===================================================================
%% POSTCONDITION
%% ===================================================================
postcondition(init, unlocked, _S, {call, _, start, []}, R) ->
    is_pid(R);
postcondition(_From, error, _S, _Call, R) ->
    case R of
      {'EXIT', _} -> true;
      _ -> false
    end;
postcondition(_From, _To, _S, {call, _, lock, [_]},
	      R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end;
postcondition(_From, _To, _S, {call, _, start, [_]},
	      R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end;
postcondition(_From, _To, _S, {call, _, stop, []}, R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end;
postcondition(_From, _To, _S, {call, _, unlock, [_]},
	      R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end.

prop_locker() ->
    ?FORALL(Cmds, (commands(?MODULE)),
	    begin
	      {History, S, Res} = run_commands(?MODULE, Cmds),
	      cleanup(S),
	      ?WHENFAIL((bluefringe_fsm:pp_eunit(locker,
						 eqc_statem:zip(Cmds,
								[R
								 || {_, R}
									<- History]))),
			(Res == ok))
	    end).

%%====================================================================
%% FSM API
%%====================================================================

lock(X1) -> catch locker:lock(X1).

start(X1) -> catch locker:start(X1).

stop() -> catch locker:stop().

unlock(X1) -> catch locker:unlock(X1).

cleanup(_S) -> catch begin stop() end.

-endif. % (EQC).
-endif. % (TEST).
