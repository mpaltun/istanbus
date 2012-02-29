%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the istanbus_web application.

-module(istanbus_web_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for istanbus_web.
start(_Type, _StartArgs) ->
    istanbus_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for istanbus_web.
stop(_State) ->
    ok.
