-module(istanbus_core_howtogo_module).

-export([load/2]).

load(From, To) ->
    {ok, Client} = thrift_client_util:new("127.0.0.1",
                                           9090,
                                           recommendationService_thrift,
                                           []),
    {Client2, {ok, Response}} = thrift_client:call(Client, recommend, [From, To]),
    %Result = {struct, [{from, list_to_binary(From)}, {to, list_to_binary(To)}]},
    {_, ok} = thrift_client:close(Client2),
    Response.
