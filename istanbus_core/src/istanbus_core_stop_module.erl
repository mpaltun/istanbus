-module(istanbus_core_stop_module).

-export([load_by_id/1, search/1, find_closest/2]).

load_by_id(StopId)  ->
    Stop = get_first(emongo:find_one(pool_mongo, "stop", [{"id", StopId}], [{fieldsnoid, ["id", "name", "location", "bus"]}])),
    Stop.
    

search(Keywords)     ->
    Query = prepare_query(Keywords, []),
    emongo:find(pool_mongo, "stop", Query, [{limit, 20}, {fieldsnoid, ["id", "name"]}]).

find_closest(Lat, Lon) ->
    Query = [{"location", [{near, [Lat, Lon]}]}],
    Fields =  ["id", "name", location],
    emongo:find(pool_mongo, "stop", Query , [{limit, 5}, {fieldsnoid, Fields}]).

% internal api
get_first([H | _]) ->
    H;
get_first([]) ->
    [].


prepare_query([], Queries) ->
    Queries;

prepare_query([Keyword | Keywords], Queries) ->
    Query = {"words", Keyword},
    prepare_query(Keywords, [Query | Queries]).

result([]) ->
    [];
result([ LocationQuery | _]) ->
    emongo:find(pool_mongo, "stop2", LocationQuery, [{limit, 1}, {fieldsnoid, ["id", "name", "latitude", "longitude"]}]).
