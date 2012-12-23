%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_howtogo_resource).
-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    From = wrq:path_info(from, ReqData),
    To = wrq:path_info(to, ReqData),
    
    DecodedFrom = http_uri:decode(From),
    DecodedTo = http_uri:decode(To),
    
    Result = istanbus_thrift_bridge:recommend(DecodedFrom, DecodedTo),
    {Result, ReqData, Context}.
