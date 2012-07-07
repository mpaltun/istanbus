%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_stop_resource).
-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    StopId = wrq:path_info(id, ReqData),
    DecodedStopId = unicode:characters_to_list(list_to_binary(http_uri:decode(StopId)), utf8),
    Stop = istanbus_core_stop_module:load_by_id(DecodedStopId),
    {mochijson2:encode(Stop),  ReqData, Context}.
