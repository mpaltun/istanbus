-module(istanbus_core_place_module).

-export([load_by_id/1]).

load_by_id(PlaceId) ->
    Result = emongo:find_one(pool_mongo, "places", [{"id", PlaceId}],
        [{fieldsnoid, ["id", "name", "display_name", "latitude", "longitude", "class", "type", "boundingbox"]}]),
    mongo_utils:get_first(Result).