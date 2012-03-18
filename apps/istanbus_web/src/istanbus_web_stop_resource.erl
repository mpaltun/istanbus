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
    case wrq:path_info(action, ReqData) of
        undefined       ->
            { "request not supported", ReqData, Context };
        "id"            ->
            StopId = wrq:path_info(param, ReqData),
            Stop = istanbus_core_stop_module:load_by_id(StopId),
            { mochijson2:encode(Stop),  ReqData, Context };
        "search"        ->
            Keyword = wrq:path_info(param, ReqData),
            Stops = istanbus_core_stop_module:search(Keyword),
            { mochijson2:encode(Stops),  ReqData, Context }
    end.
