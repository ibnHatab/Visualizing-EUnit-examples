%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%
%%% @end
%%% Created : 17 Dec 2012 by vlad <lib.aca55a@gmail.com>

-module(erlware_tests).

-compile(export_all).

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%% --------------------------------------------------------------------------------
%% Erlware Dictionary
%% --------------------------------------------------------------------------------

%% I can put arbitrary terms into the dictionary as keys
key() -> union([integer(), atom()]).

%% I can put arbitrary terms into the dictionary as values
value() -> union([integer(), atom(), binary(), boolean(), string()]).


sym_dict() ->
    ?SIZED(Size,sym_dict(Size)).

sym_dict(0) ->
    {'$call', ec_dictionary, new, [ec_gb_trees]};
sym_dict(Size) ->
    ?LAZY(frequency([
		     {1, {'$call', ec_dictionary, remove, [key(), sym_dict(Size - 1)]}},
		     {2, {'$call', ec_dictionary, add, [value(), value(), sym_dict(Size - 1)]}}
		    ])).

%% Is it realy work?
%% proper_gen:sample(erlware_tests:value()).
%% proper_gen:sample(erlware_tests:key()).
%% proper_gen:sample(erlware_tests:sym_dict()).
%% {ok,D} = proper_gen:pick(erlware_tests:sym_dict(),10).

%% When I put a value in the dictionary by a key, I can retrieve that same value
prop_get_after_add() ->
    ?FORALL({Dict,K,V}, {sym_dict(), key(), value()},
            V =:= ec_dictionary:get(K, ec_dictionary:add(K, V, Dict))
           ).

%% When I put a different value in the dictionary by key it does not change other key value pairs.
prop_add_twice_get_first() ->
    ?FORALL({Dict,K1,V1,K2,V2}, {sym_dict(), key(), value(), key(), value()},
            ?IMPLIES(K1 /= K2,
                     V1 =:= ec_dictionary:get(K1,
                                              ec_dictionary:add(K2, V2,
                                                                ec_dictionary:add(K1, V1, Dict))) 
                    )).

%% When I update a value the new value in available by the new key
prop_update_the_value() ->
    ?FORALL({Dict,K,V1,V2}, {sym_dict(), key(), value(), value()},
                V2 =:= ec_dictionary:get(K,
                                         ec_dictionary:add(K, V2,
                                                           ec_dictionary:add(K, V1, Dict)))
           ).

%% When a value does not exist a not found exception is created
does_not_exist_exception_test() ->
    ?assertThrow(not_found, ec_dictionary:get(42, ec_dictionary:new(ec_gb_trees))). 





%% dict() ->
%%     ?SIZED(Size,dict(Size)).

%% dict(0) ->
%%     ec_dictionary:new(ec_gb_trees);
%% dict(Size) ->
%%     ?LET(D,dict(Size-1),
%% 	 frequency([
%% 		    {1, dict(0)},
%% 		    {3, ec_dictionary:remove(integer(), D)},
%% 		    {6, ec_dictionary:add(integer(), integer(), D)}
%% 		   ])).



-endif. % (TEST).



