-module(istanbus_core_bus_module).

-export([load_all/1, load_by_id/2]).

load_all(MongoPool) ->
    emongo:find(MongoPool, "bus", [], [{ fields, ["_id"]}]).

load_by_id(MongoPool, BusId) ->
    Results = emongo:find(MongoPool, "bus", [{"_id", BusId}]),
    get_first(Results).

get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.
