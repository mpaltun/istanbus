-module(istanbus_thrift_bridge).

-export([find_path/2, search/2]).

find_path(From, To) ->
  Cache = emongo:find_one(pool_mongo, "suggestions", [{"from", From}, {"to", To}], [{fieldsnoid, ["json"]}]),
  Result = case Cache of
    [] ->
      Json = find_path_from_java_api(From, To),
      %% let it cache :)
      emongo:insert(pool_mongo, "suggestions", [{"from", From}, {"to", To}, {"json", Json}]),
      Json;
    Doc ->
      mongo_utils:get_first(Doc, <<"json">>)
  end,
  Result.

search(Index, Keyword) ->
  Opts = [{strict_read, false}, {strict_write, false}, {framed, true}],
  {ok, Client} = thrift_client_util:new("127.0.0.1", 9090, istanbusJavaService_thrift, Opts),
  {Client2, {ok, Response}} = thrift_client:call(Client, search, [Index, Keyword]),
  {_, ok} = thrift_client:close(Client2),
  Response.

find_path_from_java_api(From, To) ->
  Opts = [{strict_read, false}, {strict_write, false}, {framed, true}],
  {ok, Client} = thrift_client_util:new("127.0.0.1", 9090, istanbusJavaService_thrift, Opts),
  {Client2, {ok, Response}} = thrift_client:call(Client, recommend, [From, To]),
  {_, ok} = thrift_client:close(Client2),
  Response.