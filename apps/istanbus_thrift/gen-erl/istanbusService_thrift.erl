%%
%% Autogenerated by Thrift Compiler (0.8.0)
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(istanbusService_thrift).
-behaviour(thrift_service).


-include("istanbusService_thrift.hrl").

-export([struct_info/1, function_info/2]).

struct_info('i am a dummy struct') -> undefined.
%%% interface
% recommend(This, From_stop, To_stop)
function_info('recommend', params_type) ->
  {struct, [{1, string}, {2, string}]}
;
function_info('recommend', reply_type) ->
  {list, {list, {list, string}}};
function_info('recommend', exceptions) ->
  {struct, []}
;
% get_closest_stops(This, Latitude, Longitude)
function_info('get_closest_stops', params_type) ->
  {struct, [{1, string}, {2, string}]}
;
function_info('get_closest_stops', reply_type) ->
  string;
function_info('get_closest_stops', exceptions) ->
  {struct, []}
;
function_info(_Func, _Info) -> no_function.

