%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_find_place_path_resource).

-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    FromLatitude = list_to_float(wrq:path_info(from_latitude, ReqData)),
    FromLongitude = list_to_float(wrq:path_info(from_longitude, ReqData)),
    
    ToLatitude = list_to_float(wrq:path_info(to_latitude, ReqData)),
    ToLongitude = list_to_float(wrq:path_info(to_longitude, ReqData)),
    
    Result = istanbus_core_path_module:find_path(FromLatitude, FromLongitude, ToLatitude, ToLongitude),
    {Result, ReqData, Context}.
