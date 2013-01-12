%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2013, vlad
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2013 by vlad <lib.aca55a@gmail.com>

-module(encode_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([bl/0]).
-compile(export_all).

%% Example of triangulation test
prop_t2b_b2t() ->
    ?FORALL(T, term(), T =:= binary_to_term(term_to_binary(T))).


longlist(G) ->
    ?SIZED(Size, resize(Size * 20, list(resize(Size, G)))).

-type bl() :: binary() | [1..255].
bl() -> union([binary(), longlist(range(1,255))]).

prop_enc_dec() ->
  ?FORALL(Msg, bl(),
	  begin
	      EncDecMsg = base64:decode(base64:encode(Msg)),
	      case is_binary(Msg) of
		  true  -> EncDecMsg =:= Msg;
		  false -> EncDecMsg =:= list_to_binary(Msg)
	      end
	  end).

