%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  Testing hungry creature state jumping
%%% @end
%%% Created :  8 Dec 2012 by vlad <lib.aca55a@gmail.com>

-module(creature_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("femto_test/include/eunit_fsm.hrl").

-define(SECRET, cheese_day).


%% + start stop start stop
start_stop_test_() ->
    {inorder,
     [ ?_assertMatch({ok, _}, creature:start_link(?SECRET)),
       ?_assertMatch(ok, creature:stop()),
       ?_assertMatch({ok, _}, creature:start_link(?SECRET)),
       ?_assertMatch(ok, creature:stop())]}.

%% - stop
stop_first_test() ->
    ?assertExit({noproc, _}, creature:stop()).

%% + start new_day stop
new_day_CW_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertMatch(ok, creature:new_day(lettuce)),
      ?_assertMatch(ok, creature:new_day(grapes)),
      ?_assertMatch(ok, creature:new_day(cheese)),
      ?_assertMatch(ok, creature:new_day(lettuce))
     ]
    }.

%% + start new_day stop
new_day_UCW_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertMatch(ok, creature:new_day(grapes)),
      ?_assertMatch(ok, creature:new_day(lettuce)),
      ?_assertMatch(ok, creature:new_day(cheese)),
      ?_assertMatch(ok, creature:new_day(grapes))
     ]
    }.


%% + start lettuce stop
start_lettuce_stop_test() ->
    ?assertMatch({ok, _}, creature:start_link(?SECRET)),
    ?assertMatch(ok, creature:new_day(lettuce)),
    ?assertMatch(ok, creature:stop()),
    ?assertMatch({ok, _}, creature:start_link(?SECRET)),
    ?assertMatch(ok, creature:stop()).


%% + start grapes stop
start_grapes_stop_test() ->
    ?assertMatch({ok, _}, creature:start_link(?SECRET)),
    ?assertMatch(ok, creature:new_day(grapes)),
    ?assertMatch(ok, creature:stop()),
    ?assertMatch({ok, _}, creature:start_link(?SECRET)),
    ?assertMatch(ok, creature:stop()).


%% - start(cheese) new_day(cheese)
cheese_day_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertExit(error, ?exit_on_error(creature:new_day(cheese),
					 {error,indigent_food}))
     ]}.

cheese_day1_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertMatch(ok, creature:new_day(grapes)),
      ?_assertMatch(ok, creature:new_day(cheese)),
      ?_assertExit(error, ?exit_on_error(creature:new_day(cheese),
					 {error,indigent_food}))
     ]}.


cheese1_day2_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertMatch(ok, creature:new_day(lettuce)),
      ?_assertMatch(ok, creature:new_day(cheese)),
      ?_assertExit(error, ?exit_on_error(creature:new_day(cheese),
					 {error,indigent_food}))
     ]}.

%% - start(lettuce) new_day(lettuce)
lettuce_day_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertMatch(ok, creature:new_day(lettuce)),
      ?_assertExit(error, ?exit_on_error(creature:new_day(lettuce),
					 {error,indigent_food}))
     ]}.

lettuce_day1_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertMatch(ok, creature:new_day(grapes)),
      ?_assertMatch(ok, creature:new_day(lettuce)),
      ?_assertExit(error, ?exit_on_error(creature:new_day(lettuce),
					 {error,indigent_food}))
     ]}.

%% - start(grapes) new_day(grapes)
grapes_day_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertMatch(ok, creature:new_day(grapes)),
      ?_assertExit(error, ?exit_on_error(creature:new_day(grapes),
					 {error,indigent_food}))
     ]}.

grapes_day1_test_() ->
    {setup,
     fun ()  -> creature:start_link(?SECRET) end,
     fun (_) -> creature:stop() end,
     [
      ?_assertMatch(ok, creature:new_day(lettuce)),
      ?_assertMatch(ok, creature:new_day(grapes)),
      ?_assertExit(error, ?exit_on_error(creature:new_day(grapes),
					 {error,indigent_food}))
     ]}.

%% Locker running, this doesn't impact BlueFringe tests
%% fsm_state_test_() ->
%%     {foreach,
%%      fun ()  -> {ok, P} = creature:start_link(?SECRET), P end,
%%      fun (_) -> creature:stop() end,
%%      [
%%       ?_fsm_test(whereis(creature),"Lettice Day Test",
%%       		 [
%%       		  {call,creature,new_day,[lettuce],ok},
%%       		  {state,is,lettuce_day},
%%       		  {call,creature,new_day,[lettuce],{error,indigent_food}},
%%       		  {state,is,lettuce_day}, 	%check Frame
%%       		  {loopdata,is,[5,5,5]},
%% 		  {call,creature,hungry,[],{lettuce_left,5}},
%% 		  {loopdata,is,[5,4,5]}
%%       		 ]),
%%       ?_fsm_test(whereis(creature),"Provisioning Day Test",
%%       		 [
%%       		  {state,is,cheese_day},
%%       		  {loopdata,is,[5,5,5]},
%% 		  {call,creature,buy,[lettuce, 2], ok},
%%       		  {loopdata,is,[5,7,5]}
%% 		 ])
%%      ]
%%     }.
