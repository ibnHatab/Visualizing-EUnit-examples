-module(tr69_filter).

-export([start/0, start/1,
         clear/1]).

start() ->
    start([
	   {actors, [client, timer, us, peer, httpc, httpd] }
	  ]).

start(ExtraOptions) ->
    Options =
        [{event_order, event_ts},
         {scale, 2},
         {max_actors, 10},
         {detail_level, 90},
         {trace_pattern, {tr69, max}},
         {trace_global, true},
         {title, "TR-069 tracer"} | ExtraOptions],
    et_viewer:start(Options).

clear(P) ->
    ColPid = et_viewer:get_collector_pid(P),
    ok = et_collector:clear_table(ColPid).
