%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level application module.
%%% @author snorecone
%%% @copyright Apache 2.0
%%% @date 2022-07-06
%%% @end
%%%-----------------------------------------------------------------------------

-module(elasticsearch_app).
-author(snorecone).

-behavior(application).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([
    start/2,
    stop/1
]).

%%%=============================================================================
%%% API
%%%=============================================================================

start(_StartType, _StartArgs) ->
    {ok, _Pid} = start_elasticsearch_httpc_profile(),
    lager:debug("Elastic Search Erlang started"),
    elasticsearch_sup:start_link().

stop(_State) ->
    ok.

%%%=============================================================================
%%% Internal
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% start inets with an elasticsearch profile, possibly with configuration options
%% @end
%%------------------------------------------------------------------------------
start_elasticsearch_httpc_profile() ->
    inets:start(httpc, [{profile, elasticsearch}]).