-module(istanbus_core_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, save_snippet/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    emongo:add_pool(pool_mongo, "localhost", 27017, "istanbus_2012-02-23", 1),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [pool_mongo], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

save_snippet(Snippet) ->
    gen_server:call(?SERVER, {save_snippet, Snippet}, infinity).

handle_call({save_snippet, Snippet}, _From, MongoPool) ->
    SavedSnippet = istanbus_snippet:save(MongoPool, Snippet),
    {reply, SavedSnippet, MongoPool};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

