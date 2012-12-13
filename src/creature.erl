%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  Creature eating cheese, lettuce and grapes
%%% @end
%%% Created :  8 Dec 2012 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(creature).

-behaviour(gen_fsm).

%% API
-export([start_link/1, stop/0,
	hungry/0, buy/2, new_day/1]).

%% gen_fsm callbacks
-export([init/1,
	 cheese_day/2, cheese_day/3,
	 lettuce_day/2, lettuce_day/3,
	 grapes_day/2, grapes_day/3,
	 handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).


-type quantity() :: non_neg_integer().

-record(storage, { cheese  = 5 :: quantity(),
		   lettuce = 5 :: quantity(),
		   grapes  = 5 :: quantity()
		 }).

%%%===================================================================
%%% API
%%%===================================================================
-type food_left() :: 'cheese_left' | 'lettece_left' | 'grapes_left'.

%% -spec hungry() :: 
hungry() ->
    gen_fsm:sync_send_event(?MODULE, eat).

buy(Food, Quantity) ->
    gen_fsm:send_event(?MODULE, {store, Food, Quantity}).

new_day(Food) ->
    gen_fsm:sync_send_event(?MODULE, {new_day, Food}).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Day) ->
    io:format(user, "~n---------------------------~n", []),
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Day], []).

stop() ->
    io:format(user, "~n============================~n", []),
    gen_fsm:sync_send_all_state_event(?SERVER, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Day]) ->
    {ok, Day, #storage{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec cheese_day(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
cheese_day({store, Food, Quantity}, State) ->
    NewState = handle_store(Food, Quantity, State),    
    {next_state, cheese_day, NewState};
cheese_day(_Event, State) ->
    {next_state, cheese_day, State}.

lettuce_day({store, Food, Quantity}, State) ->
    NewState = handle_store(Food, Quantity, State),    
    {next_state, lettuce_day, NewState};
lettuce_day(_Event, State) ->
    {next_state, cheese_day, State}.

grapes_day({store, Food, Quantity}, State) ->
    NewState = handle_store(Food, Quantity, State),    
    {next_state, grapes_day, NewState};
grapes_day(_Event, State) ->
    {next_state, cheese_day, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec cheese_day(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
cheese_day(eat, From, #storage{cheese = Cheese} = State) ->
    gen_fsm:reply(From, {cheese_left, Cheese}),
    {next_state, cheese_day, State#storage{cheese = Cheese - 1}};

cheese_day({new_day, cheese}, _From, State) ->
    {reply, {error, indigent_food}, cheese_day, State};
cheese_day({new_day, lettuce}, _From, State) ->
    {reply, ok, lettuce_day, State};
cheese_day({new_day, grapes}, _From, State) ->
    {reply, ok, grapes_day, State};

cheese_day(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, cheese_day, State}.

lettuce_day(eat, From, #storage{lettuce = Lettuce} = State) ->
    gen_fsm:reply(From, {lettuce_left, Lettuce}),
    {next_state, lettuce_day, State#storage{lettuce = Lettuce - 1}};

lettuce_day({new_day, cheese}, _From, State) ->
    {reply, ok, cheese_day, State};
lettuce_day({new_day, lettuce}, _From, State) ->
    {reply, {error, indigent_food}, lettuce_day, State};
lettuce_day({new_day, grapes}, _From, State) ->
    {reply, ok, grapes_day, State};

lettuce_day(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, lettuce_day, State}.

grapes_day(eat, From, #storage{grapes = Grapes} = State) ->
    gen_fsm:reply(From, {grapes_left, Grapes}),
    {next_state, grapes_day, State#storage{grapes = Grapes - 1}};

grapes_day({new_day, cheese}, _From, State) ->
    {reply, ok, cheese_day, State};
grapes_day({new_day, lettuce}, _From, State) ->
    {reply, ok, lettuce_day, State};
grapes_day({new_day, grapes}, _From, State) ->
    {reply, {error, indigent_food}, grapes_day, State};

grapes_day(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, grapes_day, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(stop, _From, _StateName, State) ->
    {stop, normal, ok, State};
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc By some `Quantity of `Food from store
%% @end
%%--------------------------------------------------------------------
handle_store(Food, Quantity, S) ->
    case Food of
	cheese ->
	    S#storage{cheese = S#storage.cheese + Quantity};
	lettuce ->
	    S#storage{lettuce = S#storage.lettuce + Quantity};
	grapes ->
	    S#storage{grapes = S#storage.grapes + Quantity};
	_ ->
	    S
    end.
    
