-module(istanbus_core_stop_module).

-export([load_by_id/1, search/1, search_and_get_location/1]).

load_by_id(StopId)  ->
    get_first(emongo:find_one(pool_mongo, "stop", [{"id", StopId}], [{fieldsnoid, ["id", "name", "bus_list"]}])).

search(Keywords)     ->
    Query = prepare_query(Keywords, []),
    emongo:find(pool_mongo, "stop", Query, [{limit, 20}, {fieldsnoid, ["id", "name"]}]).

search_and_get_location(Keywords)   ->
    Query = prepare_query(Keywords, []),
    Result = emongo:find(pool_mongo, "stop", Query, [{limit, 1}, {fieldsnoid, ["id"]}]),
    result(Result).

% internal api
get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.


prepare_query([], Queries) ->
    Queries;

prepare_query([Keyword | Keywords], Queries) ->
    Query = {"words", Keyword},
    prepare_query(Keywords, [Query | Queries]).

result([]) ->
    [];
result([ LocationQuery | _]) ->
    emongo:find(pool_mongo, "stop2", LocationQuery, [{limit, 1}, {fieldsnoid, ["id", "latitude", "longitude"]}]).
