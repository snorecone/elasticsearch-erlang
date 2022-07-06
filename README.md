<h1 align='center'>
 	elasticsearch-erlang
</h1>

<h2 align='center'>
	An erlang client for elasticsearch's REST interface. Just the bare minimum for now.
</h2>

<br>

<p align='center'>
  <img src="https://img.shields.io/badge/License-Apache_2.0-blue.svg"/>
  <img src="https://badgen.net/badge/Open%20Source%20%3F/Yes%21/blue?icon=github)](https://github.com/Naereen/badges/"/>
</p>

<br>

<h2> Other elasticsearch clients and why this was made </h2>

* [erlasticsearch](https://github.com/dieswaytoofast/erlasticsearch) - too much frustration with thrift.
* [erlastic_search](https://github.com/tsloughter/erlastic_search) - dependency on hackney for http requests (built-in pooling is broken); unable to configure without wrapping the module.

<h2>Config</h2>

Default config is shown below. You can supply your own config in sys.config, but must be of this form.

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
    ]},

- worker_impl: This is the default worker, but you can supply your own. Must conform to poolboy worker behaviour specs.

<h2> Usage </h2>

Create index:

	elasticsearch:create_index(<<"my_index">>).

Add to index using jsx to convert map:

	elasticsearch:index(<<"my_index">>, <<"_doc">>, #{<<"my_key">> => <<"my_value">>}).

Add to index using binary query:

	elasticsearch:index(<<"my_index">>, <<"_doc">>, <<"{\"my_key\": \"my_value\"}">>).

Add to index with id:

	elasticsearch:index(<<"my_index">>, <<"_doc">>, <<"id">>, #{<<"my_key">> => <<"my_value">>}).

Get document:

    elasticsearch:get_document(<<"my_index">>, <<"_doc">>, <<"id">>).

Add in bulk using jsx to convert map:

    elasticsearch:index(<<"bank">>, <<"_bulk">>, [#{ <<"index">> => #{<<"_index">> => <<"abc">>} }, #{<<"my_key5">> => <<"my_value5">>}, #{ <<"index">> => #{<<"_index">> => <<"abc">>} }, #{<<"my_key6">> => <<"my_value6">>}, #{ <<"index">> => #{<<"_index">> => <<"abc">>} }, #{<<"my_key7">> => <<"my_value7">>}]).

Add in bulk using binary query:

    elasticsearch:index(<<"bank">>, <<"_bulk">>, <<"{ \"index\": {\"_index\": \"abc\"} }\n{\"my_key5\": \"my_value5\"}\n{ \"index\": {\"_index\": \"abc\"} }\n{\"my_key6\": \"my_value6\"}\n{ \"index\": {\"_index\": \"abc\"} }\n{\"my_key7\": \"my_value7\"}\n">>).

Search:

	elasticsearch:search(<<"my_index">>, <<"_doc">>, #{<<"query">> => #{<<"match">> => #{<<"key1">> => <<"value1">>}}}).

Count:

	elasticsearch:count(<<"my_index">>, <<"_doc">>, #{<<"query">> => #{<<"match">> => #{<<"key1">> => <<"value1">>}}}).
