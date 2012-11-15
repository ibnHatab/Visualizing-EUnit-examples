%%% File    : tr69.erl
%%% Description :
%%%   Manual startup of the TR-069 application


-module(tr69).

-compile({parse_transform, lager_transform}).


-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, Reason} ->
	    erlang:error({app_start_failed, App, Reason}),
            {error, Reason}
    end.


%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ok = ensure_started(sasl),
    ok = ensure_started(lager),
    ok = ensure_started(gproc),
    acs_core_sup:start_link().

%% @spec start() -> ok
%% @doc Start the tr69 for testing.
start() ->
    ok = ensure_started(sasl),
    ok = lager:start(),
    ok = ensure_started(gproc),
    ok = application:start(tr69).

%% @spec stop() -> ok
%% @doc Stop the acs_core server.
stop() ->
    Res = application:stop(tr69),
    application:stop(gproc),
    application:stop(lager),
    application:stop(sasl),
    Res.

