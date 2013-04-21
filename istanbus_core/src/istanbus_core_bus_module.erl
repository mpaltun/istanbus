-module(istanbus_core_bus_module).

-export([load_all/0, load_by_id/1, load_stops/2, load_time_sheet/1]).

load_all() ->
    Result = load_by_id("all"),
    [{_, _}, {_, {array, BusList}}] = Result,
    BusList.

load_by_id(BusId) ->
    Result = emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, ["id", "name", "stops", "timesheet", "notes"]}]),
    mongo_utils:get_first(Result).

load_stops(BusId, Direction) when Direction =:= "go" orelse Direction =:= "turn" ->
    %% Direction go | turn
    Field = "stops." ++ Direction,
    Result = load_bus_with_fields(BusId, [Field]),
    mongo_utils:get_first(Result, <<"stops">>);

load_stops(_BusId, _Direction) ->
    [].

load_time_sheet(BusId) ->
    Result = load_bus_with_fields(BusId, ["timesheet"]),
    mongo_utils:get_first(Result, <<"timesheet">>).

% internal api
load_bus_with_fields(BusId, Fields) ->
    emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, Fields}]).
