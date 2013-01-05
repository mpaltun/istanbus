%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_bus_stops_resource).
-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

content_types_provided(ReqData, Context) ->
  {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
  Bus_id = wrq:path_info(id, ReqData),
  Direction = wrq:path_info(direction, ReqData),
  DecodedBusId = unicode:characters_to_list(list_to_binary(http_uri:decode(Bus_id)), utf8),
  Stops = istanbus_core_bus_module:load_stops(DecodedBusId, Direction),
  {mochijson2:encode(Stops), ReqData, Context}.