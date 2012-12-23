-module(istanbus_thrift_bridge).

-export([recommend/2, stop_search/1]).

recommend(From, To) ->
  Opts = [{strict_read, false}, {strict_write, false}, {framed, true}],
  {ok, Client} = thrift_client_util:new("127.0.0.1", 9090, istanbusJavaService_thrift, Opts),
  {Client2, {ok, Response}} = thrift_client:call(Client, recommend, [From, To]),
  {_, ok} = thrift_client:close(Client2),
  Response.

stop_search(Keyword) ->
  Opts = [{strict_read, false}, {strict_write, false}, {framed, true}],
  {ok, Client} = thrift_client_util:new("127.0.0.1", 9090, istanbusJavaService_thrift, Opts),
  {Client2, {ok, Response}} = thrift_client:call(Client, stop_search, [Keyword]),
  {_, ok} = thrift_client:close(Client2),
  Response.