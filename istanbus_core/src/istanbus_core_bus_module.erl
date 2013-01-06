-module(istanbus_core_bus_module).

-export([load_all/0, load_by_id/1, load_stops/2, load_timesheet/1, load_by_stop/1]).

load_all() ->
    Result = load_by_id("all"),
    [{_, _}, {_, {array, BusList}}] = Result,
    BusList.

load_by_id(BusId) ->
    Result = emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, ["id", "name", "stops_go", "time", "stops_turn", "notes"]}]),
    get_first(Result).

load_stops(BusId, Direction) when Direction =:= "go" orelse Direction =:= "turn" ->
    %% Direction go | turn
    Field = "stops_" ++ Direction,
    Result = load_bus_with_fields(BusId, [Field]),
    get_first(Result, list_to_binary(Field));

load_stops(_BusId, _Direction) ->
    [].

load_timesheet(BusId) ->
    Result = load_bus_with_fields(BusId, ["time"]),
    get_first(Result, <<"time">>).

load_by_stop(StopId) ->
    Query = {"$or", [[{"stops_go.id", StopId}], [{"stops_turn.id", StopId}]]},
    emongo:find(pool_mongo, "bus", [Query], [{fieldsnoid, ["id", "name"]}]).

% internal api

get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.


get_first([H | _], Field) ->
    proplists:get_value(Field, H);
get_first([], _Field) ->
    [].

load_bus_with_fields(BusId, Fields) ->
    emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, Fields}]).
