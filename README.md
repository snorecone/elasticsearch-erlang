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

Return:

	{ok,#{<<"acknowledged">> => true,<<"index">> => <<"my_index">>, 
	  <<"shards_acknowledged">> => true}}

Add to index:

	elasticsearch:index(<<"my_index">>, <<"type">>, [{<<"key1">>, <<"value1">>}]).

Return:

	{ok,#{<<"_id">> => <<"q_xZ04EBi8Z_kFa_Foev">>,
      <<"_index">> => <<"my_index">>,<<"_primary_term">> => 1,
      <<"_seq_no">> => 0,
      <<"_shards">> =>
          #{<<"failed">> => 0,<<"successful">> => 1,<<"total">> => 2},
      <<"_type">> => <<"type">>,<<"_version">> => 1,
      <<"result">> => <<"created">>}}

Search:

	elasticsearch:search(<<"my_index">>, <<"type">>, #{<<"query">> => #{<<"match">> => #{<<"key1">> => <<"value1">>}}}).

Return:

	{ok,#{<<"_shards">> =>
          #{<<"failed">> => 0,<<"skipped">> => 0,<<"successful">> => 1,
            <<"total">> => 1},
      <<"hits">> =>
          #{<<"hits">> =>
                [#{<<"_id">> => <<"q_xZ04EBi8Z_kFa_Foev">>,
                   <<"_index">> => <<"my_index">>,<<"_score">> => 0.2876821,
                   <<"_source">> => #{<<"key1">> => <<"value1">>},
                   <<"_type">> => <<"type">>}],
            <<"max_score">> => 0.2876821,
            <<"total">> =>
                #{<<"relation">> => <<"eq">>,<<"value">> => 1}},
      <<"timed_out">> => false,<<"took">> => 8}}

Count:

	elasticsearch:count(<<"my_index">>, <<"type">>, #{<<"query">> => #{<<"match">> => #{<<"key1">> => <<"value1">>}}}).

Return:

	{ok,#{<<"_shards">> =>
          #{<<"failed">> => 0,<<"skipped">> => 0,<<"successful">> => 1,
            <<"total">> => 1},
      <<"hits">> =>
          #{<<"hits">> =>
                [#{<<"_id">> => <<"q_xZ04EBi8Z_kFa_Foev">>,
                   <<"_index">> => <<"my_index">>,<<"_score">> => 0.2876821,
                   <<"_source">> => #{<<"key1">> => <<"value1">>},
                   <<"_type">> => <<"type">>}],
            <<"max_score">> => 0.2876821,
            <<"total">> =>
                #{<<"relation">> => <<"eq">>,<<"value">> => 1}},
      <<"timed_out">> => false,<<"took">> => 2}}
