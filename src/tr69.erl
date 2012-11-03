%%% File    : tr69.erl
%%% Description : 
%%%   Manual startup of the TR-069 application


-module(tr69).

-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

-export([report_event/4,
	 report_event/5]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    acs_core_sup:start_link().

%% @spec start() -> ok
%% @doc Start the acs_core server.
start() ->
    ensure_started(sasl),
    ensure_started(gproc),
    ensure_started(lager),
    application:start(tr69).

%% @spec stop() -> ok
%% @doc Stop the acs_core server.
stop() ->
    Res = application:stop(tr69),
    application:stop(lager),
    application:stop(gproc),
    application:stop(sasl),
    Res.

%% ET 
report_event(DetailLevel, FromTo, Label, Contents) ->
    %% N.B External call
    ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).

report_event(_DetailLevel, _From, _To, _Label, _Contents) ->
    hopefully_traced.

