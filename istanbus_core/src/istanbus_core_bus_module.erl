-module(istanbus_core_bus_module).

-export([load_all/0, load_by_id/1, load_stopsgo/1,
            load_stopscome/1, load_timesheet/1, load_by_stop/1]).

load_all() ->
    Result = load_by_id("all"),
    [{_, _}, {_, {array, BusList}}] = Result,
    BusList.

load_by_id(BusId) ->
    Result = emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, ["id", "name", "stops_go", "time", "stops_come", "notes"]}]),
    get_first(Result).

load_stopscome(BusId) ->
    Result = load_bus_with_fields(BusId, ["stops_come"]),
    get_first(Result, <<"stops_come">>).

load_stopsgo(BusId) ->
    Result = load_bus_with_fields(BusId, ["stops_go"]),
    get_first(Result, <<"stops_go">>).

load_timesheet(BusId) ->
    Result = load_bus_with_fields(BusId, ["time"]),
    get_first(Result, <<"time">>).

load_by_stop(StopId) ->
    % db.bus.find({"stops_go.id" : "A0280"}, {id : 1, _id : 0});
    emongo:find(pool_mongo, "bus", [{"stops_go.id", StopId}], [{fieldsnoid, ["id", "name"]}]).

% internal api

get_first(Result) ->
    get_first(Result, null).


get_first([H | _], null) ->
    H;
get_first([H | _], Field) ->
    proplists:get_value(Field, H);
get_first([], _Field) ->
    {struct, []}.

load_bus_with_fields(BusId, Fields) ->
    emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, Fields}]).
