%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_bus_resource).
-export([init/1, to_json/2, content_types_provided/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    case wrq:path_info(id, ReqData) of
        BusId ->
			Cache = get_the_cache(),
            DecodedBusId = unicode:characters_to_list(list_to_binary(http_uri:decode(BusId)), utf8),
            Cache ! {get, DecodedBusId, self()},
            receive
                error ->
                    Bus = istanbus_core_bus_module:load_by_id(DecodedBusId),
                    Cache = get_the_cache(),
                    Cache ! {put, DecodedBusId, Bus},
                    {mochijson2:encode(Bus), ReqData, Context};
                {ok, Bus} ->
                    {mochijson2:encode(Bus), ReqData, Context}
            end
    end.

cache(Dict) ->
    receive
        {get, Key, Pid} ->
            Pid ! dict:find(Key, Dict),
            cache(Dict);
        {put, Key, Value} ->
            Dict1 = dict:store(Key, Value, Dict),
            cache(Dict1)
    end.

get_the_cache() ->
    % does the cache exists?
    Pid = whereis(cache_pid),
    if
        is_pid(Pid) ->
            % found
            Pid;
        true ->
            erlang:display("cache started"),
            % not found, create it
            NewPid = spawn(fun() ->
                cache(dict:new())
            end),
            register(cache_pid, NewPid),
            NewPid
    end.
