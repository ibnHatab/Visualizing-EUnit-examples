%%% File    : tradepost_tests.erl
%%% Description : Tradepost test module

-module(tradepost_tests_v0).

-include_lib("eunit/include/eunit.hrl").
-include_lib("femto_test/include/eunit_fsm.hrl").

-import(tradepost, [which_tp/0, start_tp/0, stop_tp/0,
		    seller_identify_tp/1, seller_insertitem_tp/2, withdraw_item_tp/1]).


%%--------------------------------------------------------------------
%% @doc
%% Test tradepost API via FSM inspection mechanism
%% V.0 unitary test
%% @end
%%--------------------------------------------------------------------
fsm_tradepost_test_() ->
    {foreach,
     fun ()  -> start_tp()end,
     fun (_) -> stop_tp() end,
     [
      % Initialy in pending state and no loop data
      ?_fsm_state(which_tp(), pending),
      ?_fsm_data(which_tp(), [undefined,undefined,undefined,undefined,undefined]),
      %% From Pending, identify seller, then state should be pending
      %% loopdata should now contain seller_password
      ?_fsm_test(which_tp(), "Identify seler Test",
		 [
		  {call, tradepost, seller_identify, [which_tp(), seller_password], ok},
		  {state, is, pending},
		  {loopdata, is, [undefined,undefined, seller_password, undefined,undefined]}
		 ]),
      ?_fsm_test(which_tp(), "Insert/Withdraw Test",
		 [
		  {call, tradepost, seller_identify, [which_tp(), seller_password], ok},
		  {state, is, pending},
		  {loopdata, is, [undefined,undefined, seller_password, undefined,undefined]},

		  {call, tradepost, seller_insertitem, [which_tp(), playstation, seller_password], ok},
		  {state, is, item_received},	% mfa
		  {loopdata, is, [playstation, undefined, seller_password, undefined,undefined]},
		  {call, tradepost, withdraw_item, [which_tp(), seller_password], ok},
		  {state, is, pending}
		 ])
     ]
    }.

%%--------------------------------------------------------------------
%% @doc
%% Design Proofing with FSM Unitary tests 
%% V.1 fsm unitary test for Gramma Inference using positive and negative tests
%% @end
%%--------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -define(VZTEST, true).
-ifdef(VZTEST).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% + start stop start stop
start_stop_start_test_() ->
    {inorder,
     [ ?_assertMatch(true,start_tp()),
       ?_assertMatch(ok,stop_tp()),
       ?_assertMatch(true,start_tp()),
       ?_assertMatch(ok,stop_tp())
      ]}. 

%% - unregister stop
stopFirst_test_() ->
    [
    ?_assertError(badarg,stop_tp()),
    ?_assertError(badarg,stop_tp())
    ].

%% - start start
startTwice_test_() ->
      [{setup,
	fun ()  -> start_tp () end,
	fun (_) -> stop_tp() end,
	?_assertError(badarg,start_tp())
   }].

%% - identify
identify_test() ->
    ?assertExit({noproc,{gen_fsm,sync_send_event,_}},
 		tradepost:seller_identify_tp(seller_password)).

%% + start identify stop
identify_stop_test_()->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     ?_assertMatch(ok,seller_identify_tp(seller_password))
    }.

%% - start insertitem
insertitem_test_() ->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
      ?_assertExit(error,
		   case seller_insertitem_tp(playstation,seller_password) of
		       error -> exit(error); Other -> Other end)
    }.


%% + start identify insertitem withdraw insertitem
identify_recall_item_test_()->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     [
      ?_test(
	 begin
	     ?assertMatch(ok, seller_identify_tp(seller_password)),
	     ?assertMatch(ok, tradepost:seller_insertitem_tp(playstation,seller_password)),
	     ?assertMatch(ok, tradepost:withdraw_item_tp(seller_password))
	     %% ?assertMatch(ok, tradepost:seller_insertitem_tp(playstation,seller_password))
	 end
	)]}.    
      

%% - start identify insertitem withdraw withdraw 
identify_withdraw_twice_test_()->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     [
      ?_test(
	 begin
	     ?assertMatch(ok, seller_identify_tp(seller_password)),
	     ?assertMatch(ok, tradepost:seller_insertitem_tp(playstation,seller_password)),
	     ?assertMatch(ok, tradepost:withdraw_item_tp(seller_password)),
	     ?assertError(value,
			  case tradepost:withdraw_item_tp(seller_password) of
			      error ->  .error(value); Other -> Other end
			 )
	 end
	)]}.    
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-endif.						% VZTEST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

