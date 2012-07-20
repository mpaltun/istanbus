%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_bus_stopsgo_resource).
-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    case wrq:path_info(id, ReqData) of
        BusId ->
			DecodedBusId = unicode:characters_to_list(list_to_binary(http_uri:decode(BusId)), utf8),
            Stops = istanbus_core_bus_module:load_stopsgo(DecodedBusId),
            {mochijson2:encode(Stops), ReqData, Context}
    end.
