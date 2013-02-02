-module(istanbus_core_bus_module).

-export([load_all/0, load_by_id/1, load_stops/2, load_timesheet/1]).

load_all() ->
    Result = load_by_id("all"),
    [{_, _}, {_, {array, BusList}}] = Result,
    BusList.

load_by_id(BusId) ->
    Result = emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, ["id", "name", "stops", "timesheet", "notes"]}]),
    get_first(Result).

load_stops(BusId, Direction) when Direction =:= "go" orelse Direction =:= "turn" ->
    %% Direction go | turn
    Field = "stops." ++ Direction,
    Result = load_bus_with_fields(BusId, [Field]),
    get_first(Result, <<"stops">>);

load_stops(_BusId, _Direction) ->
    [].

load_timesheet(BusId) ->
    Result = load_bus_with_fields(BusId, ["timesheet"]),
    get_first(Result, <<"timesheet">>).

% internal api

get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.

get_first([H | _], Field) ->
    Value = proplists:get_value(Field, H),
    case Value of
        undefined -> [];
        _ -> Value
    end;
get_first([], _Field) ->
    [].

load_bus_with_fields(BusId, Fields) ->
    emongo:find_one(pool_mongo, "bus", [{"id", BusId}], [{fieldsnoid, Fields}]).
