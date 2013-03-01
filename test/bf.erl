%%% @author Andreea NICOLA <lib.aca55a@gmail.com>
%%% @copyright (C) 2013, Andreea NICOLA
%%% @doc
%%%
%%% @end
%%% Created :  4 Jan 2013 by Andreea NICOLA <lib.aca55a@gmail.com>

-module(bf).

-export([compile/2]).


compile(File_Name, Options) ->
    {{Pos,Neg},_CleanupTree} = eunit_to_fsm:dynamic(File_Name,
						    [
						     {i, "ebin"},
						     {i, "../.."},
						     {i, "deps/*/ebin"},
						     {i, "../deps"},
						     {i, "../../../deps"},
						     {i, "../include"},
						     {i, "./include"}
						     | Options
						    ]
						  ),
    %% io:format(user, "Pos: ~p~n", [Pos]),
    %% io:format(user, "Neg: ~p~n", [Neg]),
    Automata = bluefringe:qsm({Pos,Neg}),
    bluefringe_dot:visualize(Automata).


