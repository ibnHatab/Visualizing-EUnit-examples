%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2012, vlad
%%% @doc
%%%
%%% @end
%%% Created : 15 Dec 2012 by vlad <lib.aca55a@gmail.com>

-module(types_tests).

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([leazy_dict/0, model/1]).

ok_prop_comute() ->
    ?FORALL({I, J}, {nat(), nat()},
	    (I + J) =:= (J + I)).


%% proper:quickcheck(erlware_tests:prop_sorted())

gen_list_indexes() ->
    ?SUCHTHAT({I, J, List}, {nat(), nat(), list(integer())},
	      J < length(List) andalso I < length(List)).

ok_prop_sorted() ->
    ?FORALL({I, J, List}, gen_list_indexes(),
	    begin
		SList = lists:sort(List),
		?IMPLIES(I < J andalso I > 0,
			 lists:nth(I, SList) =< lists:nth(J, SList)
			)
	    end
	   ).

%% --------------------------------------------------------------------------------
%% http://www.erlang-factory.com/upload/presentations/193/EUC2009-QuickCheck.pdf
%% --------------------------------------------------------------------------------

%% \/ {I,L} E {int() x list(int())} |- (L - I) => not I in L
ok_prop_delete() ->
    ?FORALL({I,L}, {integer(), list(integer())},
            collect(lists:member(I,L),
                    ?IMPLIES(no_duplicate(L),
                             not lists:member(I, lists:delete(I, L))
                            ))
           ).

no_duplicate(L) ->
    lists:sort(L) =:= lists:usort(L).
%% --
ok_prop_delete_from_ulist() ->
    ?FORALL({I, UL}, {integer(), ulist(integer())},
            not lists:member(I, lists:delete(I, UL))
           ).

ulist(Elem) ->
    ?LET(L,list(Elem), lists:usort(L)).
%% --

ok_prop_delete_if_in_list() ->
    fails(
      ?FORALL(L, non_empty(list(integer())),
              ?FORALL(I, elements(L),
                      not lists:member(I, lists:delete(I, L))
                     ))).

%% --


%% -------------------------------------------------------------------------------
%% Dictionary
%% --------------------------------------------------------------------------------

ok_prop_unique_key() ->
    ?FORALL(D, dict(integer(),float()),
            no_duplicate(dict:fetch_keys(D))
           ).

%% --
key() ->
    integer().
value() ->
    float().

leazy_dict() ->
    ?LAZY(oneof([dict:new(),
                 ?LET({K, V, D}, {key(), value(), leazy_dict()},
                      dict:store(K,V,D)
                     )
           ])).

sym_dict() ->
    ?LAZY(oneof([
                 {call, dict, new, []},
                 ?LETSHRINK([D], [sym_dict()],
                            {call, dict, store, [key(), value(), D]})
                ])).

ok_prop_unique_key_sym() ->
    ?FORALL(D, sym_dict(),
            no_duplicate(dict:fetch_keys(eval(D)))
           ).
%% --

model(Dict) ->
    dict:to_list(Dict).

ok_prop_store() ->
    ?FORALL({K,V,D}, {key(),value(),sym_dict()},
            begin
                Dict = eval(D),
                M = model(dict:store(K,V,Dict)),
                M1 = model_store(K,V,model(Dict)),
                io:format(">> ~p ~p ~n", [M, M1]),
                M == M1
            end
           ).

model_store(K,V,PL) ->
    proplists:normalize([proplists:property(K,V) | PL]). 





-endif. % (TEST).


%% prop_new_array_opts() ->
%%     ?FORALL(Opts, array:array_opts(),
%% 	    array:is_array(array:new(Opts))).


%% -spec divide(integer(), integer()) -> integer().
%% divide(A, B) ->
%%   A div B.

%% devide_test_() ->
%%     ?_assert(proper:check_spec([?MODULE, devide, []])).

%% -spec filter(fun((T) -> term()), [T]) -> [T].
%% filter(Fun, List) ->
%%     lists:filter(Fun, List).

%% -spec max([T]) -> T.
%% max(List) ->
%%     lists:max(List).
