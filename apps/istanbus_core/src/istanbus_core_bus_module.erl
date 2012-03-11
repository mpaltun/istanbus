-module(istanbus_core_bus_module).

-export([load_all/0, load_by_id/1]).

load_all() ->
    Result = load_by_id("all"),
    [{_, _}, {_, {array, BusList}}] = Result,
    BusList.

load_by_id(BusId) ->
    Result = emongo:find_one(pool_mongo, "bus", [{"_id", BusId}]),
    get_first(Result).

% internal api
get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.
