%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2012 by vlad <lib.aca55a@gmail.com>

-module(tr69_trace_test).


-include_lib("eunit/include/eunit.hrl").

-include("tr69.hrl").

-export([send_trace/0]).

report_event_test_() ->
    { setup,
      fun () ->
	      ok = lager:start(),
	      lager:set_loglevel(lager_console_backend, info)
      end,
      fun (_O)->
	      ok
      end,

      [
       ?_test(begin
       		  tr69_trace:enable(max, all),
       		  tr69_trace:disable()
       		      ,
       		  tr69_trace:enable(max, all),
       		  tr69_trace:disable()
       	      end)
       ,
       ?_test(begin
       		   tr69_trace:enable(max, all)
       		       , tr69_trace:report_event(50, ?MODULE,  "START", "UTEST")
       		       , tr69_trace:report_event(50, ?MODULE, http,  'Message', "args")
       		       , tr69_trace:report_event(50, ?MODULE,  "Label", "Contents")
       		       , tr69_trace:disable()
       	       end)
       ,
       ?_test(begin

       		  lager:set_loglevel(lager_console_backend, error),

       		  tr69_trace:enable(max, all)
       		      , tr69_trace:report_event(50, ?MODULE,  "lager:set_loglevel", "lager_console_backend, error")
       		      , tr69_trace:disable()
       		      , lager:set_loglevel(lager_console_backend, info)
       	       end)
       ,
       ?_test(begin
       		  tr69_trace:enable(max, all),
       		  tr69_trace:report_event(50, ?MODULE,  "separator",
       					  "--------------------------------------"),
       		  tr69_trace:report_event(50, cpe, "not a module", "? error"),
       		  tr69_trace:disable()
       	      end)
       ,

       %% enable EV
       %% ?_test(begin
       %% 		  tr69_filter:start(),
       %% 		  tr69_trace:enable(max, all),

       %% 		  send_trace(),

       %% 		  tr69_utils:sleep(5000),
       %% 		  tr69_filter:stop(),
       %% 		  tr69_trace:disable(),
       %% 		  ok
       %% 	      end)
       %% ,

       %% enable per module
       ?_test(begin
       		  tr69_trace:enable(max, cwmp_cli),
       		  tr69_trace:report_event(50,  cwmp_cli, "cwmp_cli", "? OK"),
       		  tr69_trace:disable()
       	      end)
       ,

       %% enable level
       ?_test(begin
       		  tr69_trace:enable(60, cwmp_cli),
       		  tr69_trace:report_event(50,  cwmp_cli, "cwmp_cli", "? OK"),
       		  tr69_trace:report_event(70,  cwmp_cli, "cwmp_cli", "? NOK"),
       		  tr69_trace:disable()
       	      end)

      ]

    }.

send_trace() ->
    Events = [
	      % cwmp_cli, cpe_rpc, cpe_rpc_session, cpe_http, cpe_rpc_session, ibrowse
	      {cwmp_cli,cpe_rpc,open},
	      {cpe_rpc, cpe_http, open},
	      {cpe_http,ibrowse,start},
	      {cpe_http,cpe_rpc_session,new},
	      {cpe_rpc,cpe_rpc_session,new},
	      {cwmp_cli,cpe_rpc_session,push}
	     ],
    [tr69_trace:report_event(60, F, T, L, [F, T, L])
     || {F,T,L} <- Events].



