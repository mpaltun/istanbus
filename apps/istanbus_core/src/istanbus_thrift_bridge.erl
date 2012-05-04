-module(istanbus_thrift_bridge).

-export([recommend/2, get_closest_stops/2]).

recommend(From, To) ->
    {ok, Client} = thrift_client_util:new("127.0.0.1", 9090, istanbusService_thrift, []),
    {Client2, {ok, "ok"}} = thrift_client:call(Client, recommend, [From, To]),
    %Result = {struct, [{from, list_to_binary(From)}, {to, list_to_binary(To)}]},
    {_, ok} = thrift_client:close(Client2),
    Recommendations = emongo:find_one(pool_mongo, "howtogo", [{"_id", From ++ "_" ++ To}]),
    Recommendations.

get_closest_stops(Latitude, Longitude) ->
    {ok, Client} = thrift_client_util:new("127.0.0.1", 9090, istanbusService_thrift,[]),
    {Client2, {ok, Response}} = thrift_client:call(Client, get_closest_stops, [Latitude, Longitude]),
    {_, ok} = thrift_client:close(Client2),
    Response.
