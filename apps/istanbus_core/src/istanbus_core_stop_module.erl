-module(istanbus_core_stop_module).

-export([load_by_id/1]).

load_by_id(StopId) ->
    get_first(emongo:find_one(pool_mongo, "stop", [{"_id", StopId}])).

% internal api
get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.
