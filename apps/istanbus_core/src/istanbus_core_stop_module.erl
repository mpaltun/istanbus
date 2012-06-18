-module(istanbus_core_stop_module).

-export([load_by_id/1, search/1]).

load_by_id(StopId)  ->
    get_first(emongo:find_one(pool_mongo, "stop", [{"_id", StopId}], [{fields, ["name", "bus_list"]}])).
search(Keywords)     ->
    Query = prepare_query(Keywords, []),
    emongo:find(pool_mongo, "stop", Query, [{limit, 20}, {fields, ["name"]}]).

% internal api
get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.


prepare_query([], Queries) ->
    Queries;

prepare_query([Keyword | Keywords], Queries) ->
    Query = {"words",{regexp, [$^ | Keyword], []}},
    prepare_query(Keywords, [Query | Queries]).
