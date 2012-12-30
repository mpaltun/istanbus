-module(istanbus_core_bus_module).

-export([load_all/0, load_by_id/1, load_stopsgo/1,
            load_stopscome/1, load_timesheet/1]).

load_all() ->
    Result = load_by_id("all"),
    [{_, _}, {_, {array, BusList}}] = Result,
    BusList.

load_by_id(BusId) ->
    Result = emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, ["id", "stops_go", "time", "stops_come", "notes"]}]),
    get_first(Result).

load_stopscome(BusId) ->
    Result = load_bus_with_fields(BusId, ["stops_come"]),
    proplists:get_value(<<"stops_come">>, get_first(Result)).

load_stopsgo(BusId) ->
    Result = load_bus_with_fields(BusId, ["stops_go"]),
    proplists:get_value(<<"stops_go">>, get_first(Result)).

load_timesheet(BusId) ->
    Result = load_bus_with_fields(BusId, ["time"]),
    proplists:get_value(<<"time">>, get_first(Result)).
% internal api
get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.

load_bus_with_fields(BusId, Fields) ->
    emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, Fields}]).
