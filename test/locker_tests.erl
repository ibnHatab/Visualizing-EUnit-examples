%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%
%%% @end
%%% Created :  6 Dec 2012 by vlad <lib.aca55a@gmail.com>

-module(locker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("femto_test/include/eunit_fsm.hrl").



-import(locker,[start/1, stop/0]).


-define(SECRET, 42).

%% %% Four positive tests.
%% %% + start stop start stop
start_stop_test_() ->
    {inorder,
     [ ?_assertMatch({ok, _},start(?SECRET)),
       ?_assertMatch(ok,stop()),
       ?_assertMatch({ok, _},start(?SECRET)),
       ?_assertMatch(ok,stop())]}.

%% - stop
stop_first_test() ->
    ?assertExit({noproc, _},stop()).


%% %% - start start
start_twice_test_() ->
  {setup,
    fun ()  -> start(?SECRET) end,
    fun (_) -> stop() end,
      ?_assertExit(error, case start(?SECRET) of
			      {error,{already_started,_}} -> exit(error);
			      Other -> Other
			  end)
   }.

%% + start lock unlock lock stop
lock_unlock_test_() ->
    {setup,
     fun ()  -> start(?SECRET) end,
     fun (_) -> stop() end,
     [
      ?_assertMatch(ok, locker:lock(?SECRET)),
      ?_assertMatch(ok, locker:unlock(?SECRET)),
      ?_assertMatch(ok, locker:lock(?SECRET)),
      ?_assertMatch(ok, locker:unlock(?SECRET))
     ]
    }.

%% + start lock stop
lock_stop_test_() ->
    {setup,
     fun ()  -> start(?SECRET) end,
     fun (_) -> stop() end,
     [
      ?_assertMatch(ok, locker:lock(?SECRET)),
      ?_assertMatch(ok, stop()),
      ?_assertMatch({ok, _}, locker:start(?SECRET))
     ]
    }.

%% - start unlock
start_unlock_test_() ->
    {setup,
     fun ()  -> start(?SECRET) end,
     fun (_) -> stop() end,
     [
      ?_assertExit(error, case locker:unlock(?SECRET) of
			      {error, invalid_message} -> exit(error);
			      Other -> Other
			  end)
     ]
    }.

%% - start lock start
start_lock_test_() ->
    {setup,
     fun ()  -> start(?SECRET) end,
     fun (_) -> stop() end,
     [
      ?_assertMatch(ok, locker:lock(?SECRET)),
      ?_assertExit(error, case start(?SECRET) of
			      {error,{already_started,_}} -> exit(error);
			      Other -> Other
			  end)
     ]
    }.


%% Locker running, this doesn't impact BlueFringe tests
fsm_state_test_() ->
    {foreach, 
     fun ()  -> {ok, P} = start(?SECRET), P end,
     fun (_) -> stop() end,
     [
      ?_fsm_state(whereis(locker), unlocked),
      ?_fsm_data(whereis(locker), [?SECRET]),
      ?_fsm_test(whereis(locker),"Deposit something Test",
	 	[
		 {call,locker,lock,[?SECRET],ok},
		 {state,is,locked},
		 {call,locker,unlock,[?SECRET],ok},
		 {state,is,unlocked}
		])
     ]  
    }.
