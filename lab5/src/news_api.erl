-module(news_api).

-export([get_latest/0, get_latest/1, get_latest_topic/1]).

get_latest() ->
  RequestBody =
    #{<<"lang">> => <<"en">>,
      <<"page_size">> => 5,
      <<"page">> => 1},
  Request =
    {os:getenv("NEWS_BASE_URL") ++ "latest_headlines",
     [{"x-api-key", os:getenv("NEWS_API_KEY")}],
     "application/json",
     jsx:encode(RequestBody)},
  io:format("NEWS API get_latest | Request: ~p~n", [Request]),
  {ok, {{_, StatusCode, _}, Headers, ResponseBody}} = httpc:request(post, Request, [], []),
  io:format("NEWS API get_latest | response: ~p~n",
            [{ok, StatusCode, Headers, ResponseBody}]),
  case StatusCode of
    200 ->
      {ok, parse_news(ResponseBody)};
    _ ->
      io:format("NEWS API get_latest | Error, status code: ~p~n", [StatusCode]),
      api_error
  end.

get_latest(Keywords) ->
  RequestBody =
    #{<<"q">> => list_to_binary(Keywords),
      <<"lang">> => <<"en">>,
      <<"page_size">> => 5,
      <<"page">> => 1},
  Request =
    {os:getenv("NEWS_BASE_URL") ++ "search",
     [{"x-api-key", os:getenv("NEWS_API_KEY")}],
     "application/json",
     jsx:encode(RequestBody)},
  io:format("NEWS API get_latest | Request: ~p~n", [Request]),
  {ok, {{_, StatusCode, _}, Headers, ResponseBody}} = httpc:request(post, Request, [], []),
  io:format("NEWS API get_latest | response: ~p~n",
            [{ok, StatusCode, Headers, ResponseBody}]),
  case StatusCode of
    200 ->
      {ok, parse_news(ResponseBody)};
    _ ->
      io:format("NEWS API get_latest | Error, status code: ~p~n", [StatusCode]),
      api_error
  end.

get_latest_topic(Topic) ->
  RequestBody =
    #{<<"topic">> => list_to_binary(Topic),
      <<"lang">> => <<"en">>,
      <<"page_size">> => 5,
      <<"page">> => 1},
  Request =
    {os:getenv("NEWS_BASE_URL") ++ "latest_headlines",
     [{"x-api-key", os:getenv("NEWS_API_KEY")}],
     "application/json",
     jsx:encode(RequestBody)},
  io:format("NEWS API get_latest_topic | Request: ~p~n", [Request]),
  {ok, {{_, StatusCode, _}, Headers, ResponseBody}} = httpc:request(post, Request, [], []),
  io:format("NEWS API get_latest_topic | response: ~p~n",
            [{ok, StatusCode, Headers, ResponseBody}]),
  case StatusCode of
    200 ->
      {ok, parse_news(ResponseBody)};
    _ ->
      io:format("NEWS API get_latest_topic | Error, status code: ~p~n", [StatusCode]),
      api_error
  end.

parse_news(ResponseBody) ->
  BodyMap = jsx:decode(list_to_binary(ResponseBody)),
  case maps:is_key(<<"articles">>, BodyMap) of
    true ->
      #{<<"articles">> := ArticleList} = BodyMap,
      parse_news(ArticleList, <<>>);
    _ ->
      "No news found"
  end.

parse_news([], Acc) ->
  Acc;
parse_news(ArticleList, Acc) ->
  [Article | NextArticleList] = ArticleList,
  #{<<"title">> := Title, <<"link">> := Link} = Article,
  parse_news(NextArticleList, iolist_to_binary([Acc, Title, "\n", Link, "\n\n"])).
