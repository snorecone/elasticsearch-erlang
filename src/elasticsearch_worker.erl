-module(elasticsearch_worker).

-behavior(gen_server).

-export([
    start_link/1,
    request/4,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    base_url
}).

-define(PROFILE, elasticsearch).
-define(HTTP_OPTIONS, [{timeout, 5000}]).
-define(HTTPC_OPTIONS, [{body_format, binary}, {full_result, false}]).

%%
%% API.
%%
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

request(Method, Path, Body, Params) ->
    poolboy:transaction(elasticsearch, fun (Worker) ->
        gen_server:call(Worker, {Method, Path, Body, Params})
    end).

%%
%% gen_server
%%
init([Host, Port]) ->
    BaseUrl = lists:concat(["http://", Host, ":", to_list(Port), "/"]),
    State = #state{
        base_url = BaseUrl
    },
    {ok, State}.

handle_call({Method, Path, Body0, Params0}, _From, #state{ base_url = BaseUrl } = State) ->
    URLPath = BaseUrl ++ string:join([escape(to_list(P)) || P <- Path], "/"),
    Body = case Body0 of 
        <<>> -> <<>>;
        B    -> jsx:encode(B)
    end,
    Params = string:join([string:join([Key, Value], "=") || {Key, Value} <- Params0], "&"),
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
        ?HTTP_OPTIONS, ?HTTPC_OPTIONS, ?PROFILE) of
        {ok, {Status, RespBody}} when Status == 200; Status == 201 ->
            {ok, search_result(RespBody)};
        {ok, {_Status, RespBodyFail}} ->
            {error, RespBodyFail};
        {error, Reason} ->
            {error, Reason}
    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% private
%%
search_result(Body) ->
    Result = jsx:decode(Body),
    Result.
    
to_list(List) when is_list(List) -> List;
to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom).

escape(String) ->
    edoc_lib:escape_uri(String).
