%%% @author Gratiela Ghergu <Gratiela.Ghergu@Alcatel-Lucent.com>
%%% @copyright (C) 2012, Gratiela Ghergu
%%% @doc
%%%
%%% @end
%%% Created : 13 Nov 2012 by Gratiela Ghergu <Gratiela.Ghergu@Alcatel-Lucent.com>

-module(gproc_tests).

-include("tr69/include/tr69.hrl").


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


gproc_register_test_() ->
    { setup,
      fun () ->
	      ensure_started(gproc)
      end,
      fun (_O)->
	      application:stop(gproc)
      end,

      [
       %% register/unregister
       ?_test(begin
		  gproc:reg({n, l, "nume"}, "Grati")
	      end),
       %% set/get properties
       ?_test(begin
		  Pid = spawn_link(fun() ->
%%					   B = gproc:reg({n, l, "nume1"}, "Grati"),
					   ?DBG(self()),
					   receive
					       stop ->						   
						   ?DBG(stoped)
					   end
				   end),
		  Pid ! stop
	      end),
       %% transfer to ptocess
       ?_test(begin
		  ok
	      end)       
      ]}.

-endif.
