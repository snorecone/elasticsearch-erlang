-module(elasticsearch).

-export([
    start/0,
    transaction/1,
    create_index/1,
    create_index/3,
    index/3,
    index/4,
    index/5,
    index/6,
    update/4,
    update/5,
    update/6,
    delete/3,
    delete/4,
    delete/5,
    search/3,
    search/4,
    search/5,
    count/3,
    count/4,
    count/5
]).

start() ->
    application:start(inets),
    application:start(poolboy),
    application:start(jsx),
    application:start(elasticsearch).

transaction(Fun) ->
    poolboy:transaction(elasticsearch, Fun).

create_index(Name) ->
    create_index(Name, [], []).

create_index(Name, _Settings, _Mappings) ->
    % Body = [
    %     {settings, Settings},
    %     {mappings, Mappings}
    % ],
    poolboy:transaction(elasticsearch, fun (Worker) ->
        elasticsearch_worker:request(Worker, put, [Name], <<>>, [])
    end).

index(Index, Type, Doc) ->
    index(Index, Type, undefined, Doc, []).

index(Worker, Index, Type, Doc) when is_pid(Worker) ->
    index(Worker, Index, Type, undefined, Doc, []);
index(Index, Type, Id, Doc) when is_binary(Id) ->
    index(Index, Type, Id, Doc, []);
index(Index, Type, Doc, Params) ->
    index(Index, Type, undefined, Doc, Params).

index(Worker, Index, Type, Id, Doc) when is_pid(Worker) ->
    index(Worker, Index, Type, Id, Doc, []);
index(Index, Type, Id, Doc, Params) ->
    poolboy:transaction(elasticsearch, fun (Worker) ->
        index(Worker, Index, Type, Id, Doc, Params)
    end).

index(Worker, Index, Type, undefined, Doc, Params) ->
    elasticsearch_worker:request(Worker, post, [Index, Type], Doc, Params);
index(Worker, Index, Type, Id, Doc, Params) ->
    elasticsearch_worker:request(Worker, put, [Index, Type, Id], Doc, Params).

update(Index, Type, Id, Doc) when is_binary(Id) ->
    update(Index, Type, Id, Doc, []).

update(Worker, Index, Type, Id, Doc) when is_pid(Worker) ->
    update(Worker, Index, Type, Id, Doc, []);
update(Index, Type, Id, Doc, Params) ->
    poolboy:transaction(elasticsearch, fun (Worker) ->
        update(Worker, Index, Type, Id, Doc, Params)
    end).

update(Worker, Index, Type, Id, Doc, Params) ->
    elasticsearch_worker:request(Worker, post, [Index, Type, Id, <<"_update">>], Doc, Params).

delete(Index, Type, Id) ->
    delete(Index, Type, Id, []).

delete(Worker, Index, Type, Id) when is_pid(Worker) ->
    delete(Worker, Index, Type, Id, []);
delete(Index, Type, Id, Params) ->
    poolboy:transaction(elasticsearch, fun (Worker) ->
        delete(Worker, Index, Type, Id, Params)
    end).

delete(Worker, Index, Type, Id, Params) ->
    elasticsearch_worker:request(Worker, delete, [Index, Type, Id], <<>>, Params).

search(Index, Type, Query) ->
    search(Index, Type, Query, []).

search(Worker, Index, Type, Query) when is_pid(Worker) ->
    search(Worker, Index, Type, Query, []);
search(Index, Type, Query, Params) ->
    poolboy:transaction(elasticsearch, fun (Worker) ->
        search(Worker, Index, Type, Query, Params)
    end).

search(Worker, Index, Type, Query, Params) ->
    elasticsearch_worker:request(Worker, post, [Index, Type, <<"_search">>], Query, Params).

count(Index, Type, Query) ->
    search(Index, Type, Query, []).

count(Worker, Index, Type, Query) when is_pid(Worker) ->
    count(Worker, Index, Type, Query, []);
count(Index, Type, Query, Params) ->
    poolboy:transaction(elasticsearch, fun (Worker) ->
        count(Worker, Index, Type, Query, Params)
    end).

count(Worker, Index, Type, Query, Params) ->
    elasticsearch_worker:request(Worker, post, [Index, Type, <<"_count">>], Query, Params).
