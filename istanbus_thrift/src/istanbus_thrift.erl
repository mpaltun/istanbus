%% @author Mustafa Paltun <mpaltun@gmail.com>
%% @copyright 2012 Mustafa Paltun

%% @doc istanbus_thrift init here

-module(istanbus_thrift).
-author('Mustafa Paltun <mpaltun@gmail.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    istanbus_thrift_sup:start_link().

%% @spec start() -> ok
%% @doc Start the istanbus_thrift.
start() ->
    application:start(istanbus_thrift).

%% @spec stop() -> ok
%% @doc Stop the istanbus_thrift.
stop() ->
    Res = application:stop(istanbus_thrift),
    Res.
