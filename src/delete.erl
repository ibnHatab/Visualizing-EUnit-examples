%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2012 by vlad <lib.aca55a@gmail.com>

-module(delete).

-export([delete/2]).

-include_lib("proper/include/proper.hrl").

-type tree(T) :: 'leaf' | {'node', T, tree(T), tree(T)}.

tree() -> 
    ?SIZED(Size,tree(Size)).

tree(0) ->
    'leaf';
tree(Size) ->
    weighted_union(
      [
       {1, ?LAZY(tree(0))},
       {5, ?LAZY(?LETSHRINK([L,R],[tree(Size div 2),tree(Size div 2)],
			    {'node', integer(), L, R}))}]).



delete(X, L) ->
    delete(X, L, []).

delete(_, [], Acc) ->
    lists:reverse(Acc);
delete(X, [X|Rest], Acc) ->
    lists:reverse(Acc) ++ Rest;
delete(X, [Y|Rest], Acc) ->
    delete(X, Rest, [Y|Acc]).



prop_delete_removes_every_el() ->
    ?FORALL({X, L}, {integer(), list(integer())},
	   not lists:member(X, delete(X, L))).
    
