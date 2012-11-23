%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%%-----------------------------------------------------------------------------

-ifndef(tr69_hrl).
-define(tr69_hrl, true).


%% Test debug macro
-ifdef(TEST).
-define(DBG(ARG), io:format(user, "~n>> ~p: ~p~n", [??ARG, ARG])).
-else.
-define(DBG(ARG), true).
-endif.

%% Lager logging 

-define(DEBUG(Msg),
        ok = lager:debug(Msg)).
-define(DEBUG(Msg, Args),
        ok = lager:debug(Msg, Args)).

-define(INFO(Msg),
        ok = lager:info(Msg)).
-define(INFO(Msg, Args),
        ok = lager:info(Msg, Args)).

-define(WARNING(Msg),
        ok = lager:warning(Msg)).
-define(WARNING(Msg, Args),
        ok = lager:warning(Msg, Args)).

-define(ERROR(Msg),
        ok = lager:error(Msg)).
-define(ERROR(Msg, Args),
        ok = lager:error(Msg, Args)).

%% Various trace macros

-define(report(Severity, Label, Service, Content), 
	tr69_trace:report_event(Severity, Label, Service, 
				[{?MODULE, ?LINE} | [Content]])).
-define(report_important(Label, Service, Content), 
	?report(20, Label, Service, Content)).
-define(report_verbose(Label, Service, Content),   
	?report(40, Label, Service, Content)).
-define(report_debug(Label, Service, Content),     
	?report(60, Label, Service, Content)).
-define(report_trace(Label, Service, Content),     
	?report(80, Label, Service, Content)).

-define(message(Severity, From, To, Label, Content), 
	tr69_trace:report_event(Severity, From, To, Label, Content)).

%% This part should be redefined in user modules
-define(SERVICE, otp).
-define(otpri(Label, Content), ?report_important(Label, ?SERVICE, Content)).
-define(otprv(Label, Content), ?report_verbose(Label,   ?SERVICE, Content)).
-define(otprd(Label, Content), ?report_debug(Label,     ?SERVICE, Content)).
-define(otprt(Label, Content), ?report_trace(Label,     ?SERVICE, Content)).



-ifndef(TIMEON).
%% Yes, these need to be on a single line to work...
%% ?TIMEON,
%% ...some code...
%% ?TIMEOFF(my_code_block).

-define(TIMEON, erlang:put(debug_timer, [now()|case erlang:get(debug_timer) == undefined of true -> []; false -> erlang:get(debug_timer) end])).
-define(TIMEOFF(Var), lager:debug("~s :: ~10.2f ms : ~p~n", [string:copies(" ", length(erlang:get(debug_timer))), (timer:now_diff(now(), hd(erlang:get(debug_timer)))/1000), Var]), erlang:put(debug_timer, tl(erlang:get(debug_timer)))).

-define(EXPECT(Cond, Msg),
	if
	    Cond ->
		ok;
	    true ->
		ok = lager:warning("Expecting ~p: ~p at ~p:~p~n", [Msg, ??Cond, ?FILE, ?LINE])
	end).
-endif.

-endif. % -ifdef(tr69_hrl).
