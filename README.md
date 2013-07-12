# elasticsearch-erlang

An erlang client for elasticsearch's REST interface. Just the bare minimum for now.

## other elasticsearch clients and why this was made

* [erlasticsearch](https://github.com/dieswaytoofast/erlasticsearch) - too much frustration with thrift.
* [erlastic_search](https://github.com/tsloughter/erlastic_search) - dependency on hackney for http requests (built-in pooling is broken); unable to configure without wrapping the module.

