%%% File    : tr_soap_lib.erl
%%% Description : SOAP import/export routines


-module(tr_soap_lib).

-include_lib("xmerl/include/xmerl.hrl").

-include("tr69.hrl").
-include("proto.hrl").

-export([read_xml/1, write_xml/1]).

-export([return_error/2, parse_error/2, parse_error/3,
	 parse_warning/2, parse_warning/3,
	 get_QName/2, get_local_name/1,
	 local_name/1, local_ns/2,
	 xmlText/1,
	 xmlElement/1
	]).

-export([encoder/1, encode/1]).

-export([match_cwmp_ns_and_version/1, check_namespace/3]).

-import(lists, [map/2, reverse/1]).
-import(string, [join/2]).

%% @doc parse XML document from string
-spec read_xml(string()) -> Result when
      Result :: #xmlElement{} | {error, any()}.
read_xml(Str) ->
    case catch xmerl_scan:string(Str) of
	{'EXIT',Reason} ->
	    {error,Reason};
	{error,Reason}	->
	    {error,Reason};
	{ParsResult, _} -> ParsResult
	end.

-spec write_xml(Doc) -> Result when
      Doc :: #xmlElement{},
      Result :: list().
write_xml(Doc) ->
    Bin = xmerl:export_simple([Doc], xmerl_xml),
    unicode:characters_to_list(Bin).


%%%-----------------------------------------------------------------------------
%%%        SOAP Encoder
%%%-----------------------------------------------------------------------------


%% @doc Create an encoder/1 with the given options.
-spec encoder([encoder_option()]) -> function().
encoder(Options) ->
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -> soap_encode(O, State) end.

%% @doc Encode the given as SOAP to an rpc_data.
-spec encode(#rpc_data{}) -> #xmlElement{}.
encode(Any) ->
    soap_encode(Any, #encoder{}).

parse_encoder_options([], State) ->
    State;
parse_encoder_options([{version, Version} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{version=Version});
parse_encoder_options([{handler, Handler} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{handler=Handler}).

-spec soap_encode(#rpc_data{}, #encoder{}) -> #xmlElement{}.
soap_encode(_Any, #encoder{}) ->
    #xmlElement{}.



%%%-----------------------------------------------------------------------------
%% Internal API
%%%-----------------------------------------------------------------------------

-compile({nowarn_unused_function, return_error/2}).
-spec return_error(term(), any()) -> no_return().
return_error(Tag, Message) ->
    throw({error, {Tag, Message}}).

-spec parse_error(#xmlElement{}, #parser{}) -> no_return().
parse_error(Elem, State, Msg) when is_record(Elem, xmlElement)->
%    ?DBG(erlang:get_stacktrace()),
    Path = [Elem#xmlElement.name | [N || {N, _Id} <- Elem#xmlElement.parents]],
    XPath = "/" ++ join(map(fun atom_to_list/1, reverse(Path)), "/"),
    return_error(XPath, {State, Msg});
parse_error(Tag, State, Msg) ->
    return_error(Tag, {State, Msg}).


parse_error(Elem, State) ->
    parse_error(Elem, State, "Unknown element").


-spec parse_warning(#xmlElement{}, #parser{}) -> no_return().
parse_warning(Elem, State, Msg) ->
    Path = [Elem#xmlElement.name | [N || {N, _Id} <- Elem#xmlElement.parents]],
    XPath = "/" ++ join(map(fun atom_to_list/1, reverse(Path)), "/"),
    io:format(user, ":~p ~n Expect: ~p, ~p ~n", [XPath, State, Msg]).

parse_warning(Elem, State) ->
    parse_warning(Elem, State, "Unknown element").

%% Usefull filtering predicates
-compile({nowarn_unused_function, xmlText/1}).
xmlText(_=#xmlText{}) -> true;
xmlText(_) -> false.

-compile({nowarn_unused_function, xmlElement/1}).
xmlElement(_=#xmlElement{}) -> true;
xmlElement(_) -> false.

-compile({nowarn_unused_function, get_QName/2}).
get_QName(Name, Ns) when is_atom(Name), is_atom(Ns) ->
    list_to_atom(atom_to_list(Ns) ++ ":" ++ atom_to_list(Name)).

local_name(QName) when is_atom(QName) ->
    local_name(atom_to_list(QName));
local_name(QName) when is_list(QName) ->
    case lists:splitwith(fun($:) -> false;(_)->true end, QName) of
	{Ns, ":" ++ Name} -> {list_to_atom(Ns), list_to_atom(Name)};
	_ ->
	    {'', list_to_atom(QName)}
    end.

get_local_name(QName) ->
    {_Ns, Name} = local_name(QName),
    Name.

%%%-----------------------------------------------------------------------------
%% XML NS API
%%%-----------------------------------------------------------------------------

-spec check_namespace(atom(), #xmlElement{}, #parser{}) -> #parser{}.
check_namespace(RefQName, #xmlElement{name = QName} = Elem, #parser{ns=Nss} = State) ->
    {RefNsC, RefName} = local_name(RefQName),
    
    RefNs = local_ns(RefNsC, Nss),
    {Ns, Name} = local_name(QName),

    case {Name, Ns, Nss#rpc_ns.inherited}  of
	{RefName, RefNs, Ns} ->
	    State;
	{RefName, '', RefNs}->
	    State;
	{RefName, RefNs, _InhNs} -> 
	    State#parser{ ns = Nss#rpc_ns{ inherited = Ns }};
	_ ->
	    parse_warning(Elem, RefNs, "Namespace missmatch"),
	    State
    end.

local_ns('soap-env', Nss) ->  Nss#rpc_ns.ns_envelop;
local_ns('soap-enc', Nss) ->  Nss#rpc_ns.ns_encoding;
local_ns('cwmp', Nss)     ->  Nss#rpc_ns.ns_cwmp;
local_ns('xsd', Nss)      ->  Nss#rpc_ns.ns_xsd.

set_local_ns('soap-env', Nss, LocalNs) -> Nss#rpc_ns{ns_envelop = LocalNs};
set_local_ns('soap-enc', Nss, LocalNs) -> Nss#rpc_ns{ns_encoding = LocalNs};
set_local_ns('cwmp', Nss, LocalNs)     -> Nss#rpc_ns{ns_cwmp = LocalNs, cwmp_version = 1};
set_local_ns('cwmp_v2', Nss, LocalNs)  -> Nss#rpc_ns{ns_cwmp = LocalNs, cwmp_version = 2};
set_local_ns('xsd',  Nss, LocalNs)     -> Nss#rpc_ns{ns_xsd = LocalNs}.

-spec match_cwmp_ns_and_version(#xmlNamespace{}) -> {#rpc_ns{}, cwmp_version()}.
match_cwmp_ns_and_version(#xmlNamespace{default = _Default, nodes = Nodes}) ->
    NsMap = [{'soap-enc', 'http://schemas.xmlsoap.org/soap/encoding/'},
	     {'soap-env', 'http://schemas.xmlsoap.org/soap/envelope/'},
	     {'cwmp',     'urn:dslforum-org:cwmp-1-0'},
	     {'cwmp_v2',  'urn:dslforum-org:cwmp-1-2'},
	     {'xsd',      'http://www.w3.org/2001/XMLSchema-instance'},
	     {'xsd',      'http://www.w3.org/2001/XMLSchema-instance/'}],

    lists:foldl(fun({Key, URI}, Nss) ->
			case lists:keyfind(URI, 2, Nodes) of
			    false ->
				Nss;
			    {LocalNs, _U} ->
				set_local_ns(Key, Nss, list_to_atom(LocalNs))
			end
		end, #rpc_ns{}, NsMap).


%%%-----------------------------------------------------------------------------
%%% Unitary tetsts
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(XML,
	"<soap-env:Envelope " ++
	    "    xmlns:soap-enc=\"http://schemas.xmlsoap.org/soap/encoding/\" " ++
	    "    xmlns:soap-env=\"http://schemas.xmlsoap.org/soap/envelope/\"" ++
	    "    xmlns:cwmp=\"urn:dslforum-org:cwmp-1-0\">" ++
	    "   " ++
	    "  <soap-env:Header>" ++
	    "    <cwmp:ID mustUnderstand=\"1\">42</cwmp:ID>" ++
	    "    <cwmp:NoMoreRequests>0</cwmp:NoMoreRequests>" ++
	    "  </soap-env:Header>" ++
	    "  <soap-env:Body>" ++
	    "    <soap-env:Fault>" ++
	    "      <faultcode>Client</faultcode>" ++
	    "      <faultstring>CWMP fault</faultstring>" ++
	    "      <detail>" ++
	    "	<cwmp:Fault>" ++
	    "	  <FaultCode>9001</FaultCode>" ++
	    "	  <FaultString>Request Denied</FaultString>" ++
	    "	</cwmp:Fault>" ++
	    "      </detail>" ++
	    "    </soap-env:Fault>" ++
	    "  </soap-env:Body>" ++
	    "</soap-env:Envelope>"
       ).

gen_ref_doc() ->
    DocRef = tr_soap_lib:read_xml(?XML),
    DocRef.

dummy(_S) ->
    ok.

rw_xml_test_() ->
    { foreach,
      fun gen_ref_doc/0,
      fun dummy/1,
      [
       fun check_rw_id/1
      ]}.

check_rw_id(DocRef) ->
    fun() ->
	    ?assertEqual(DocRef, read_xml(write_xml(DocRef))) end.


-define(XML_NAMESPACE,
	{xmlNamespace,[],
	 [{"soapenc",'http://schemas.xmlsoap.org/soap/encoding/'},
	  {"soapenv",'http://schemas.xmlsoap.org/soap/envelope/'},
	  {"cwmp",'urn:dslforum-org:cwmp-1-0'}]}
       ).

get_ns_orts_test() ->
    Nss = match_cwmp_ns_and_version(?XML_NAMESPACE),
    ?DBG(Nss),
    ?assertEqual(undefined, Nss#rpc_ns.ns_xsd),
    ?assertEqual('soapenv', Nss#rpc_ns.ns_envelop),
    ?assertEqual('cwmp', Nss#rpc_ns.ns_cwmp),
    ?assertEqual(1, Nss#rpc_ns.cwmp_version),
    ok.


qname_test() ->
    ?assertEqual('ns:name', get_QName('name', 'ns')).


name_namespace_test() ->
    ?assertEqual({'','name'}, local_name('name')),
    ?assertEqual('name', get_local_name('ns:name')),
    
    {Name, Ns} = {'ns:name', 'ns'},
    ?assertEqual(Name, get_QName(get_local_name(Name), Ns)),
    ok.


check_namespace_test() ->
    Nss = match_cwmp_ns_and_version(?XML_NAMESPACE),
    State = check_namespace('soap-env:Envelope',
			    #xmlElement {name='soapenv:Envelope'}, #parser{ns=Nss}),
    
    ?assertEqual(Nss#rpc_ns{inherited='soapenv'}, State#parser.ns),

    ?assertEqual(State, check_namespace('soap-env:Body',
					 #xmlElement {name='soapenv:Body'}, State)),

    ?assertEqual(State, check_namespace('soap-env:Header',
					#xmlElement {name='Header'}, State)),
    
    ok.
    

    
    
-define(WHENEVER, "Hello meck").
meck_test()->
    meck:new(tr_soap),
    meck:expect(tr_soap,
		hello, 0, ?WHENEVER),
						%    ?DBG(tr_soap:hello()),
    meck:unload(tr_soap).

-endif.

