%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_bus_resource).
-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    application:start(emongo),
    emongo:add_pool(pool_mongo, "localhost", 27017, "istanbus_2012-03-02", 1),
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            All = istanbus_core_bus_module:load_all(pool_mongo),
            { mochijson2:encode(All), ReqData, Context };
        BusId ->
            Bus = istanbus_core_bus_module:load_by_id(pool_mongo, BusId),
            { mochijson2:encode(Bus),  ReqData, Context }
    end.
