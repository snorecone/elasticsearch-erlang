-module(elasticsearch_sup).

-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

%%
%% API
%%
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%
%% supervisor callbacks
%%
init([Host, Port, PoolSize, PoolMaxOverflow, HttpOptions]) ->
    PoolSpecs = [poolboy:child_spec(elasticsearch, [
            {name, {local, elasticsearch}},
            {worker_module, elasticsearch_worker},
            {size, PoolSize},
            {max_overflow, PoolMaxOverflow}
        ],
        [Host, Port, HttpOptions]
    )],
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
