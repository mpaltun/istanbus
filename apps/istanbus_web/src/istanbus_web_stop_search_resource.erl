%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 mpaltun.
%% @doc istanbus webmachine_resource.

-module(istanbus_web_stop_search_resource).
-export([init/1, to_json/2, content_types_provided/2, split/1]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    {ok, undefined}.

content_types_provided(ReqData, Context) ->
   {[{"application/json",to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
    Keyword = wrq:path_info(keyword, ReqData),
    DecodedKeyword = unicode:characters_to_list(list_to_binary(http_uri:decode(Keyword)), utf8),
    %%Keywords = re:split(DecodedKeyword, " ", [{return, list}]),
    Keywords = split(DecodedKeyword),
    Stops = istanbus_core_stop_module:search(Keywords),
    {mochijson2:encode(Stops),  ReqData, Context}.

split(Word) ->
    Strings = split(Word, [[]]),
    reverse(Strings).

split([], Parts) ->
    Parts;

split([32 | T], Parts) ->
    split(T, [[]| Parts]);

split([Char | T], Parts) ->
    [Keyword | Keywords] = Parts,
    NewKeyword = [Char | Keyword],
    split(T, [NewKeyword | Keywords]).

reverse(ListOfString) ->
    reverse_all(ListOfString, []).
 
reverse_all([], Reversed) ->
    Reversed;
reverse_all([H | T], Reversed) ->
    R = lists:reverse(H),
    reverse_all(T, [R | Reversed]).
