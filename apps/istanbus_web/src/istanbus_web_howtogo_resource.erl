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
    
    DecodedFrom = unicode:characters_to_list(list_to_binary(http_uri:decode(From)), utf8),
    DecodedTo = unicode:characters_to_list(list_to_binary(http_uri:decode(To)), utf8),
    
    Result = istanbus_core_howtogo_module:load(DecodedFrom, DecodedTo),
    {mochijson2:encode(Result), ReqData, Context}.
