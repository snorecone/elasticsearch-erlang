%%%-----------------------------------------------------------------------------
%%% @doc
%%% Poolboy worker for submitting work to elasticsearch.
%%% @author snorecone
%%% @copyright Apache 2.0
%%% @date 2022-07-06
%%% @end
%%%-----------------------------------------------------------------------------

-module(elasticsearch_worker).
-author(snorecone).

-behavior(gen_server).

%%%=============================================================================
%%% Exports and Definitions
%%%=============================================================================

%% API
-export([
    start_link/1,
    request/5
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    base_url,
    http_options
}).
-type state() :: state.

-define(PROFILE, elasticsearch).
-define(DEFAULT_HTTP_OPTIONS, [{timeout, 5000}]).
-define(HTTPC_OPTIONS, [{body_format, binary}, {full_result, false}]).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

request(Worker, Method, Path, Body, Params) ->
    gen_server:call(Worker, {Method, Path, Body, Params}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-spec init(map()) -> {ok, state()}.
init(Args) ->
    Url         = proplists:get_value(url, Args),
    HttpOptions = proplists:get_value(http_options, Args),
    Port        = proplists:get_value(port, Args),
    BaseUrl     = lists:concat(["http://", Url, ":", to_list(Port), "/"]),
    State = #state{
        base_url = BaseUrl,
        http_options = lists:ukeymerge(1, lists:usort(HttpOptions), ?DEFAULT_HTTP_OPTIONS)
    },
    lager:debug("Worker started: Url: ~s, Http Options: ~p, Port: ~p, Base Url: ~s", [Url, HttpOptions, Port, BaseUrl]),
    {ok, State}.

handle_call({Method, Path, Body0, Params0}, _From, #state{ base_url = BaseUrl, http_options = HttpOptions } = State) ->
    lager:debug("Worker received work: ~p, ~p, ~p, ~p", [Method, Path, Body0, Params0]),
    URLPath = BaseUrl ++ string:join([escape(to_list(P)) || P <- Path], "/"),
    Body = case Body0 of 
        <<>> -> <<>>;
        B    -> jsx:encode(B)
    end,
    Params = string:join([string:join([to_list(Key), to_list(Value)], "=") || {Key, Value} <- Params0], "&"),
    URL = if length(Params) > 0 -> lists:concat([URLPath, "?", Params]);
             true               -> URLPath
    end,
    Headers = [{"Content-Length", to_list(erlang:iolist_size(Body))}],
    Request = case Method of
        delete ->
            {URL, Headers};
        _      ->
            {URL, Headers, "application/json", to_list(Body)}
    end,
    Reply = case httpc:request(Method, Request, 
        HttpOptions, ?HTTPC_OPTIONS, ?PROFILE) of
        {ok, {Status, RespBody}} when Status == 200; Status == 201 ->
            {ok, search_result(RespBody)};
        {ok, {_Status, RespBodyFail}} ->
            {error, RespBodyFail};
        {error, Reason} ->
            {error, Reason}
    end,
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    lager:debug("Unknown call in elasticsearch_worker: ~s", [Request]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:debug("Unknown cast in elasticsearch_worker: ~s", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:debug("Unknown info in elasticsearch_worker: ~s", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    lager:debug("elasticsearch_worker terminating because: ~s", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

search_result(Body) ->
    Result = jsx:decode(Body),
    Result.
    
to_list(List)    when is_list(List)       -> List;
to_list(Binary)  when is_binary(Binary)   -> binary_to_list(Binary);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Atom)    when is_atom(Atom)       -> atom_to_list(Atom).

escape(String) ->
    edoc_lib:escape_uri(String).
