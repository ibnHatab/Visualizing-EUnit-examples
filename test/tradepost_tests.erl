-module(tradepost_tests).

-include_lib("eunit/include/eunit.hrl").

which_tp() ->
    whereis(tradepost).

start_tp() ->
    {ok, Pid} = tradepost:start_link(),
    register(tradepost, Pid).

stop_tp() ->
    Pid = which_tp(),
    %% unregister(tradepost),
    tradepost:stop(Pid).


%% + start stop
start_stop_test() ->
    ?assertMatch(true, start_tp()),
    ?assertMatch(ok, stop_tp()).

%% - stop
stop_test() ->
    ?assertExit({noproc,{gen_fsm,sync_send_all_state_event,[_,stop]}},stop_tp()).

%% - identify
identify_test() ->
    ?assertExit({noproc,{gen_fsm,sync_send_event,_}},
		tradepost:seller_identify(which_tp(),seller_password)).


%% From Pending, identify seller, then state should be pending
%% loopdata should now contain seller_password
%% + start identify insertitem withdraw_item
identify_test_() ->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     [
      ?_test(
	 begin
	     Pid = which_tp(),
	     ?assertEqual(ok,tradepost:seller_identify(Pid,seller_password)),
	     [_Object,_Cash,Seller,_Buyer,_Time] =
		 tradepost:introspection_loopdata(Pid),
	     ?assertEqual(seller_password, Seller),
	     ?assertEqual(ok, tradepost:seller_insertitem(Pid, playstation,seller_password)),
	     ?assertEqual(item_received,tradepost:introspection_statename(Pid)),
             ?assertEqual([playstation,undefined,seller_password,undefined,
			   undefined],tradepost:introspection_loopdata(Pid)),
	     ?assertEqual(ok,tradepost:withdraw_item(Pid,seller_password)),
             ?assertEqual(pending,tradepost:introspection_statename(Pid))
	 end
	),
      ?_assert(true)
     ]
    }.


%% - start insertitem
insertitem_test_() ->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
      ?_assertExit(error,
		   case tradepost:seller_insertitem(which_tp(), playstation,seller_password) of
		       error -> exit(error); Other -> Other end)
    }.

%% - start identify withdraw_item
ident_identify_test_() ->
    {setup,
     fun() -> start_tp() end,
     fun(_) -> stop_tp() end,
     [
      ?_assertEqual(ok,tradepost:seller_identify(which_tp(),seller_password)),
      ?_assertExit(error,
		   case tradepost:withdraw_item(which_tp(),seller_password) of
		       error -> exit(error); Other -> Other end)
     ]
    }.



%% fun() ->
	      %% {ok,Pid} = start_link(),
	      %% ?assertEqual(pending,tradepost:introspection_statename(Pid)),
	      %% [_Object,Cash,_Seller,_Buyer,_Time] =
	      %% 	  tradepost:introspection_loopdata(Pid),
	      %% ?assertEqual(undefined, Cash),
	      %% ?assertMatch(ok, stop(Pid))
      %% 	      ok
      %% end
 

%% - start stop stop
%% start_start_test_() ->
%%     {ok, Pid} = start_link(),
%%     ?assertMatch(ok, stop(Pid)),
%%     ?assertExit({noproc,{gen_fsm,sync_send_all_state_event,[Pid,stop]}}, stop(Pid)).


%% From Pending, identify seller, then state should be pending
%% loopdata should now contain seller_password
%% + start identify
%% identify_test() ->
%%     {ok, Pid} = tradepost:start_link(),
%%     %% pending = tradepost:introspection_statename(Pid),

%%     ?assertEqual(ok,tradepost:seller_identify(Pid,seller_password)),

%%     %% pending = tradepost:introspection_statename(Pid),
%%     %% [undefined,undefined,seller_password,undefined, undefined]
%%     %% 	= tradepost:introspection_loopdata(Pid),
%%     ?assertMatch(ok, tradepost:stop(Pid)).



% This is the main point of "entry" for my EUnit testing.
% A generator which forces setup and cleanup for each test in the testset
%% main_test_no() ->
%%     {foreach,
%%      fun setup/0,
%%      fun cleanup/1,
%%      % Note that this must be a List of TestSet or Instantiator
%%      % (I have instantiators)
%%      [
%%       % First Iteration
%%       fun started_properly/1,
%%       % Second Iteration
%%       fun identify_seller/1,
%%       fun insert_item/1,
%%       fun withdraw_item/1
%%      ]}.



% Setup and Cleanup
%% setup()      -> {ok,Pid} = tradepost:start_link(), Pid.
%% cleanup(Pid) -> tradepost:stop(Pid).

% Pure tests below
% ------------------------------------------------------------------------------
% Let's start simple, I want it to start and check that it is okay.
% I will use the introspective function for this


%% started_properly(Pid) ->
%%     fun() ->
%%             ?assertEqual(pending,tradepost:introspection_statename(Pid)),
%%             ?assertEqual([undefined,undefined,undefined,undefined,undefined],
%%                          tradepost:introspection_loopdata(Pid))
%%     end.

% Now, we are adding the Seller API tests
%% identify_seller(Pid) ->
%%     fun() ->
%%             % From Pending, identify seller, then state should be pending
%%             % loopdata should now contain seller_password
%%             ?assertEqual(pending,tradepost:introspection_statename(Pid)),
%%             ?assertEqual(ok,tradepost:seller_identify(Pid,seller_password)),
%%             ?assertEqual(pending,tradepost:introspection_statename(Pid)),
%%             ?assertEqual([undefined,undefined,seller_password,undefined,
%%                        undefined],tradepost:introspection_loopdata(Pid))
%%     end.

%% insert_item(Pid) ->
%%     fun() ->
%%             % From pending and identified seller, insert item
%%             % state should now be item_received, loopdata should now contain itm
%%             tradepost:introspection_statename(Pid),
%%             tradepost:seller_identify(Pid,seller_password),
%%             ?assertEqual(ok,tradepost:seller_insertitem(Pid,playstation,
%%                                                   seller_password)),
%%             ?assertEqual(item_received,tradepost:introspection_statename(Pid)),
%%             ?assertEqual([playstation,undefined,seller_password,undefined,
%%                        undefined],tradepost:introspection_loopdata(Pid))
%%     end.

%% withdraw_item(Pid) ->
%%     fun() ->
%%             % identified seller and inserted item, withdraw item
%%             % state should now be pending, loopdata should now contain only password
%%             tradepost:seller_identify(Pid,seller_password),
%%             tradepost:seller_insertitem(Pid,playstation,seller_password),
%%             ?assertEqual(ok,tradepost:withdraw_item(Pid,seller_password)),
%%             ?assertEqual(pending,tradepost:introspection_statename(Pid)),
%%             ?assertEqual([undefined,undefined,seller_password,undefined,
%%                        undefined],tradepost:introspection_loopdata(Pid))
%%     end.
