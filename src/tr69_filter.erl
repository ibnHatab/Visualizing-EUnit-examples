%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  Module for debug trace functions of the TR69 application
%%% @end
%%% Created : 11 Nov 2012 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(tr69_filter).

-export([start/0, start/1,
         clear/1, stop/1]).

start() ->
    start([
	   {actors, [cwmp_cli, cpe_rpc, cpe_rpc_session, cpe_http, cpe_rpc_session, ibrowse] }
	  ]).

start(ExtraOptions) ->
    Options =
        [{event_order, event_ts},
         {scale, 2},
         {max_actors, 10},
         {detail_level, 90},
         {trace_pattern, {tr69_trace, max}},
         {trace_global, true},
         {title, "CPE tracer"} | ExtraOptions],
    et_viewer:start(Options).

clear(P) ->
    ColPid = et_viewer:get_collector_pid(P),
    ok = et_collector:clear_table(ColPid).

stop(P) ->
    et_viewer:stop(P).


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("tr69/include/tr69.hrl").


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
    [tr69_trace:report_event(50, F, T, L, [])
     || {F,T,L} <- Events].


send_events_test_no() ->
    { setup,
      fun () ->
	      {ok, Pid} = start(),
	      Pid
      end,
      fun (Pid)->
	      stop(Pid)
      end,
      [?_test(begin
		  send_trace(),
		  tr69_utils:sleep(60000)
	      end)]}.


-endif.
