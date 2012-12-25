%% Code testing frequency.erl which is itself from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_tests).
-include_lib("eunit/include/eunit.hrl").
-import(frequency,[start/1, stop/0, allocate/0, deallocate/1, skip/0, use_skip/0]).


-define(FQ_POOL, [1,2]).

%% %% Four positive tests.
%% %% + start stop start stop
start_stop_test_() ->
    {inorder,
     [ ?_assertMatch(true,start(?FQ_POOL)),
       ?_assertMatch(ok,stop()),
       ?_assertMatch(true,start(?FQ_POOL)),
       ?_assertMatch(ok,stop())]}.


%% A single negative test.
%% - stop
stopFirst_test() ->
    ?assertError(badarg,stop()).

%% A fixture that also contains a negative test.
%% - start start
startTwice_test_() ->
  [{setup,
    fun ()  -> start(?FQ_POOL) end,
    fun (_) -> stop() end,
      ?_assertError(badarg,start(?FQ_POOL))
   }].


%% A single allocate
%% + start allocate deallocate allocate stop
allocate_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () ->
	     ?assertMatch({ok,1},allocate()),
	     ?assertMatch(ok,deallocate(1))
     end}.

allocate_twice_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () ->
	     ?assertMatch({ok,1},allocate()),
	     ?assertMatch({ok,2},allocate()),
	     ?assertMatch(ok,deallocate(2)),
	     ?assertMatch(ok,deallocate(1))
     end}.

%% No more resources
%% - start allocate allocate
%% out_of_frequency_test_one_fq() ->
%%     {setup,
%%      fun () -> ok end,
%%      fun(_) -> stop() end,
%%      fun () ->  ?assertMatch(true,start(?FQ_POOL)),
%% 		?assertMatch({ok,1},allocate()),
%% 		?assertError({badmatch,{error,no_frequency}},allocate())
%%      end}.

%% - deallocate
deallocate_without_start_test() ->
    ?assertError(badarg,deallocate(1)).
%% - allocate
allocate_without_start_test() ->
    ?assertError(badarg,allocate()).

%% - start deallocate
start_deallocate_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () ->  
	     ?assertError({badmatch,{error,not_allocated}}, deallocate(1))
     end
    }.

% - start allocate start
start_after_allocate_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () ->
	     ?assertMatch({ok,1},allocate()),
	     ?assertError(badarg, start(?FQ_POOL))
     end
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Two freq pool
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% + start allocate allocate deallocate allocate deallocate deallocate stop
allocate_deallocate_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () -> 
	     ?assertMatch({ok,1},allocate()),
	     ?assertMatch({ok,2},allocate()),
	     ?assertMatch(ok,deallocate(1)),
	     ?assertMatch({ok,1},allocate()),
	     ?assertMatch(ok,deallocate(1)),
	     ?assertMatch(ok,deallocate(1))
     end}.

%% - start allocate allocate allocate
out_of_frequency_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () -> 
		?assertMatch({ok,1},allocate()),
		?assertMatch({ok,2},allocate()),
		?assertError({badmatch,{error,no_frequency}},allocate())
     end}.


%% + start allocate allocate stop start
stop_allocate_start_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () -> 
	     ?assertMatch({ok,1},allocate()),
	     ?assertMatch({ok,2},allocate()),
	     ?assertMatch(ok,stop()),
	     ?assertMatch(true,start(?FQ_POOL))
     end}.


%% - start allocate deallocate deallocate
double_deallocate_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () -> 
	     ?assertMatch({ok,1},allocate()),
	     ?assertMatch(ok,deallocate(1)),
	     ?assertError({badmatch,{error,not_allocated}}, deallocate(1))
     end}.

%% - start allocate allocate start
start_after_allocate_twice_test_() ->
    {setup,
     fun () -> start(?FQ_POOL) end,
     fun(_) -> stop() end,
     fun () ->
	     ?assertMatch({ok,1},allocate()),
	     ?assertMatch({ok,2},allocate()),
	     ?assertError(badarg, start(?FQ_POOL))
     end
    }.

