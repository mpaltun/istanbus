%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_place_resource).
-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    case wrq:path_info(id, ReqData) of
        PlaceId ->
            Place = istanbus_core_place_module:load_by_id(list_to_integer(PlaceId)),
            {mochijson2:encode(Place), ReqData, Context}
    end.
