-module(elasticsearch).

-export([
    start/0,
    create_index/1,
    create_index/3,
    index/3,
    index/4,
    index/5,
    delete/3,
    delete/4,
    search/3,
    search/4,
    count/3,
    count/4
]).

start() ->
    application:start(inets),
    application:start(poolboy),
    application:start(jsx),
    application:start(elasticsearch).

create_index(Name) ->
    create_index(Name, [], []).

create_index(Name, _Settings, _Mappings) ->
    % Body = [
    %     {settings, Settings},
    %     {mappings, Mappings}
    % ],
    request(put, [Name], <<>>, []).

index(Index, Type, Doc) ->
    index(Index, Type, undefined, Doc, []).

index(Index, Type, Id, Doc) when is_binary(Id) ->
    index(Index, Type, Id, Doc, []);
index(Index, Type, Doc, Params) ->
    index(Index, Type, undefined, Doc, Params).

index(Index, Type, undefined, Doc, Params) ->
    request(post, [Index, Type], Doc, Params);
index(Index, Type, Id, Doc, Params) ->
    request(put, [Index, Type, Id], Doc, Params).

delete(Index, Type, Id) ->
    delete(Index, Type, Id, []).

delete(Index, Type, Id, Params) ->
    request(delete, [Index, Type, Id], <<>>, Params).

search(Index, Type, Query) ->
    search(Index, Type, Query, []).

search(Index, Type, Query, Params) ->
    request(post, [Index, Type, <<"_search">>], Query, Params).

count(Index, Type, Query) ->
    search(Index, Type, Query, []).

count(Index, Type, Query, Params) ->
    request(post, [Index, Type, <<"_count">>], Query, Params).

%%
%% private
%%
request(Method, Path, Body, Params) ->
    elasticsearch_worker:request(Method, Path, Body, Params).
