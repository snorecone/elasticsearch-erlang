-module(elasticsearch_app).

-behavior(application).

-export([
    start/2,
    stop/1
]).

%%
%% application callbacks
%%

start(_StartType, _StartArgs) ->
    {ok, _Pid} = start_elasticsearch_httpc_profile(),
    Host = get_env(host, "localhost"),
    Port = get_env(port, 9200),
    PoolSize = get_env(pool_size, 2),
    PoolMaxOverflow = get_env(pool_max_overflow, 10),
    HttpOptions = get_env(http_options, []),
    elasticsearch_sup:start_link([Host, Port, PoolSize, PoolMaxOverflow, HttpOptions]).

stop(_State) ->
    ok.

%%
%% private
%%
%% start inets with an elasticsearch profile
%% possibly with configuration options
start_elasticsearch_httpc_profile() ->
    inets:start(httpc, [{profile, elasticsearch}]).

get_env(Key, Default) ->
    case application:get_env(elasticsearch, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.
