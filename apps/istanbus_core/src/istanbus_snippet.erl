-module(istanbus_snippet).
-export([to_snippet/3, to_json/1, from_json/1, save/2]).

to_snippet(Title, Left, Right) ->
  {snippet,
    [
      {title, Title},
      {left, Left},
      {right, Right}
    ]
  }.

to_json({snippet, SnippetData}) ->
  to_json_internal(SnippetData).

from_json(SnippetJson) ->
  from_json_internal(SnippetJson).

save(MongoPool, Snippet={snippet, SnippetData}) ->
  case proplists:get_value(key, SnippetData, undefined) of
    undefined ->
        {snippet, SnippetData};
    ExistingKey ->
        Obj = emongo:find(MongoPool, "bus", [{"_id", SnippetData}]),
        Snippet
  end.

to_json_internal(SnippetData) ->
  istanbus_json:to_json(SnippetData, fun is_string/1).

from_json_internal(SnippetJson) ->
  {snippet, istanbus_json:from_json(SnippetJson, fun is_string/1)}.

is_string(title) -> true;
is_string(left) -> true;
is_string(right) -> true;
is_string(_) -> false.
