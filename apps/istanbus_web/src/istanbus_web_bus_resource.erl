%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_bus_resource).
-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    application:start(emongo),
    emongo:add_pool(pool_mongo, "localhost", 27017, "istanbus_2012-02-23", 1),
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    PathInfo = wrq:path_info(ReqData),
    {ok, BusId} = dict:find(key, PathInfo),
    Result = emongo:find(pool_mongo, "bus", [{"_id", BusId}]),
    {mochijson2:encode(get_first(Result)),  ReqData, Context}.

get_first([H | T]) ->
    H;
get_first([]) ->
    {struct, []}.
