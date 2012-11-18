-module(istanbus_thrift_bridge).

-export([recommend/2, where_is_my_bus/1]).

recommend(From, To) ->
    Key = From ++ "_" ++ To,
    DecodedKey = unicode:characters_to_list(list_to_binary(http_uri:decode(Key)), utf8),
    Result = emongo:find_one(pool_mongo, "howtogo", [{"id", DecodedKey}], [{fieldsnoid, ["id", "recommendations"]}]),
    case Result of
        [] ->
            {ok, Client} = thrift_client_util:new("127.0.0.1", 9090, istanbusService_thrift, []),
            {Client2, {ok, _Resp}} = thrift_client:call(Client, recommend, [From, To]),
            {_, ok} = thrift_client:close(Client2),
            get_first(emongo:find_one(pool_mongo, "howtogo", [{"id", DecodedKey}], [{fieldsnoid, ["id", "recommendations"]}]));
        _ ->
            get_first(Result)
    end.

where_is_my_bus(BusName) ->
    {ok, Client} = thrift_client_util:new("127.0.0.1", 9090, istanbusService_thrift,[]),
    {Client2, {ok, Response}} = thrift_client:call(Client, where_is_my_bus, [BusName]),
    {_, ok} = thrift_client:close(Client2),
    Response.

% internal api
get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.
