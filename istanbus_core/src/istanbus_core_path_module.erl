-module(istanbus_core_path_module).

-export([find_path/4]).

find_path(FromLatitude, FromLongitude, ToLatitude, ToLongitude) ->
    FromStops = istanbus_core_stop_module:find_closest(FromLatitude, FromLongitude),
    ToStops = istanbus_core_stop_module:find_closest(ToLatitude, ToLongitude),
    Result = [ find_path_for_stop(FromStop, ToStop) || FromStop <- FromStops, ToStop <- ToStops],
    "[" ++ mochiweb_util:join(Result, ",") ++ "]".

find_path_for_stop(FromStop, ToStop) ->
    FromStopId = proplists:get_value(<<"id">>, FromStop),
    ToStopId = proplists:get_value(<<"id">>, ToStop),
    istanbus_thrift_bridge:find_path(FromStopId, ToStopId).
