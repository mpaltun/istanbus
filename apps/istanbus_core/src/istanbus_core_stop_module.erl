-module(istanbus_core_stop_module).

-export([load_by_id/1, search/1]).

load_by_id(StopId)  ->
    get_first(emongo:find_one(pool_mongo, "stop", [{"_id", StopId}])).
search(Keyword)     ->
    emongo:find(pool_mongo, "stop", [{"words",{regexp, [ $^|Keyword], []}}], [{limit, 20}, {fields, ["name"]}]).

% internal api
get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.
