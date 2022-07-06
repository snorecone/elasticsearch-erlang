%%%-----------------------------------------------------------------------------
%%% @doc
%%% Top level application supervisor.
%%% @author snorecone
%%% @copyright Apache 2.0
%%% @date 2022-07-06
%%% @end
%%%-----------------------------------------------------------------------------

-module(elasticsearch_sup).
-author(snorecone).

-behaviour(supervisor).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

-export([
    start_link/0,
    init/1
]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> supervisor:startlink_ret(). 
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%=============================================================================
%%% supervisor callbacks
%%%=============================================================================

init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10},

    Pools = case application:get_env(elasticsearch, pools) of
        undefined ->
            {elasticsearch, [
                {pools, [
                    {elasticsearch_workers, [
                        {size,         10},
                        {max_overflow, 20}
                    ], [
                        {worker_impl,  elasticsearch_worker},
                        {url,          "localhost"},
                        {port,         9200},
                        {http_options, []}
                    ]}
                ]}
            ]};
        {ok, RespPools} ->
            RespPools
    end,

    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        WorkerImpl = proplists:get_value(worker_impl, WorkerArgs),
        PoolArgs = [{name, {local, Name}},
                    {worker_module, WorkerImpl}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

    {ok, {SupFlags, PoolSpecs}}.
