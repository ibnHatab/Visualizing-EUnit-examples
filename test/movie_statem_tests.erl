%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%  Statem testing for movies rental service using model state:
%%%      http://proper.softlab.ntua.gr/Tutorials/PropEr_testing_of_generic_servers.html
%%% @end
%%% Created : 29 Dec 2012 by vlad <lib.aca55a@gmail.com>
-module(movie_statem_tests).


-include_lib("proper/include/proper.hrl").

-behaviour(proper_statem).
-export([command/1, initial_state/0, next_state/3, postcondition/3, precondition/2]).


%% -compile(export_all).
-define(SERVER, movie_server).

-type password() :: pos_integer().
-type movie() :: atom().

-record(state,{users  :: [password()],
               rented :: [{password(), movie()}]}).

-define(NAMES, [bob, alice, john, alba, mary, ben]).

name() ->
    elements(?NAMES).

passwd(#state{users = Passwords}) ->
    elements(Passwords).

movie() ->
    elements([M || {M, _C} <- ?SERVER:available_movies()] ++ [bladerunner, inception]).

is_available(Movie, #state{rented=Rented}) ->
    Av = proplists:get_value(Movie, ?SERVER:available_movies(), -1),
    N = length([M || {_P, M} <- Rented, M =:= Movie]),
    N < Av.


%% Initialize the state
initial_state() ->
    #state{users=[], rented=[]}.

%% Command generator, S is the state
command(S) ->
    Users = (S#state.users =/= []),
    Rented = (S#state.rented =/= []),
    oneof([{call     ,?SERVER, create_account, [name()]},
           {call     ,?SERVER, ask_for_popcorn, []}] ++
              [{call ,?SERVER, delete_account, [passwd(S)]} || Users] ++
              [{call ,?SERVER, rent_dvd, [passwd(S), movie()]}  || Users] ++

              [?LET({Password,Movie}, elements(S#state.rented),
                    {call,?SERVER,return_dvd,[Password, Movie]}) || Rented]
         ).
 
%% Next state transformation, S is the current state
next_state(S,V,{call,_,create_account,_}) ->
    S#state{users = [V| S#state.users]}; % {call, erlang, hd, [[V]]}
next_state(S,_V,{call,_,delete_account,[Pass]}) ->
    S#state{users = lists:delete(Pass, S#state.users)};
next_state(S,_V,{call,_,rent_dvd,[Pass, Movie]}) ->
    case is_available(Movie, S) of
        true ->
            S#state{rented = [{Pass, Movie} | S#state.rented]};
        false ->
            S
    end;
next_state(S,_V,{call,_,return_dvd,[Pass, Movie]}) ->
    S#state{rented = lists:delete({Pass, Movie}, S#state.rented)};
next_state(S,_V,{call,_,_,_}) ->
    S.

%% Precondition, checked before command is added to the command sequence
precondition(S,{call,_,rent_dvd,[Pass, Movie]}) ->
    not lists:member({Pass, Movie}, S#state.rented);
precondition(_S,{call,_,_,_}) ->
    true.


%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
%% When creating an account, a _new_ password is always returned.
postcondition(S,{call,_,create_account,[Pass]},_Res) -> 
    not lists:member(Pass, S#state.users);
postcondition(_S,{call,_,delete_account,[_Pass]},Res) ->
    Res =:= account_deleted orelse Res =:= return_movies_first;
postcondition(S,{call,_,rent_dvd,[_Pass, Movie]},Res) ->
    case is_available(Movie, S) of
        true ->
            lists:member(Movie, Res);
        false ->
            not lists:member(Movie, Res)
    end;
postcondition(_S,{call,_,return_dvd,[_Pass, Movie]},Res) ->
    not lists:member(Movie, Res);
postcondition(_S,{call,_,_,_},_Res) ->
    true.

prop_server_works_fine() ->
    %% numtests(1000,
             ?FORALL(Cmds,commands(?MODULE),
                     ?TRAPEXIT(
                        begin                
                            ?SERVER:start_link(),
                            {H,S,Res} = run_commands(?MODULE,Cmds),
                            ?SERVER:stop(),
                            ?WHENFAIL(
                               begin
                                   io:format("Comands:\n"),
                                   lists:foreach(fun (C) -> io:format("    ~p~n", [C]) end, Cmds),
                                    io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res])
                               end,
                               Res == ok)
                        end))
            %% )
        .


