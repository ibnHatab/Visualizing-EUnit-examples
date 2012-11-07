%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%   This behavior define Protocol class.
%%%
%%%   X-Kernel Protocol Object behavior
%%%   - create_protocol
%%%   - open -> session
%%%   - open_enable
%%%   - open_done -> session
%%%   
%%% @end
%%% Created : 14 Oct 2012 by vlad <lib.aca55a@gmail.com>

-module(gen_protocol).

-type protocol() :: pid().
-type session() :: pid().

-export_type([protocol/0, session/0]).

%%% Protocol Object methods

%% create_protocol
%% This method coinside with <service>:start_link(Config).
-callback create_protocol(Config :: term()) ->
    {ok, protocol()} | {error, Reason :: term()}.

%% open
%% Active open on protocol object
-callback open(InvokingProtocol :: protocol(),
	       ParticipandSet :: {Controll :: protocol(), Data :: protocol()}) ->
    {ok, session()} | {error, Reason :: term()}.

%% open_enable
%% Request for passive open on low-level protocol object
-callback open_enable(InvokingProtocol :: protocol(),
		      ParticipandSet :: {Controll :: protocol(), Data :: protocol()}) ->
    ok | {error, Reason :: term()}.

%% open_done
%% Response for passive open on high-level protocol object
-callback open_done(InvokingProtocol :: protocol(),
		    ParticipandSet :: {Controll :: protocol(), Data :: protocol()}) ->
    {ok, session()} | {error, Reason :: term()}.
