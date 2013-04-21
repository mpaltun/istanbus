%% Copyright
-module(mongo_utils).
-author("mustafa").

%% API
-export([get_first/1, get_first/2]).

get_first([H | _]) ->
    H;
get_first([]) ->
    {struct, []}.

get_first([H | _], Field) ->
    Value = proplists:get_value(Field, H),
    case Value of
        undefined -> [];
        _ -> Value
    end;
get_first([], _Field) ->
    [].