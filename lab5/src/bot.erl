-module(bot).

-export([init_bot/0, send_message/2, send_topics/1]).

init_bot() ->
  init_request(),
  set_commands(),
  ok.

set_commands() ->
  RequestBody =
    #{<<"commands">> =>
        [#{<<"command">> => <<"/start">>, <<"description">> => <<"Show the greeting">>},
         #{<<"command">> => <<"/latest_news">>,
           <<"description">> => <<"Show latest news (up to 5 links), optionally add keywords">>},
         #{<<"command">> => <<"/latest_news_topic">>,
           <<"description">> => <<"Search latest news by available topics">>},
         #{<<"command">> => <<"/save_news">>,
           <<"description">> => <<"Provide a news URL to save for later">>},
         #{<<"command">> => <<"/saved_news">>, <<"description">> => <<"Show saved news URLs">>},
         #{<<"command">> => <<"/delete_saved">>,
           <<"description">> => <<"Delete the provided URL from saved">>},
         #{<<"command">> => <<"/delete_all_saved">>,
           <<"description">> => <<"Delete all saved URLs">>}]},

  Request =
    {os:getenv("BASE_URL") ++ "setMyCommands",
     [],
     "application/json",
     jsx:encode(RequestBody)},
  io:format("BOT set_commands | Request: ~p~n", [Request]),
  {ok, {{_, StatusCode, _}, Headers, ResponseBody}} = httpc:request(post, Request, [], []),
  io:format("BOT set_commands | response: ~p~n", [{ok, StatusCode, Headers, ResponseBody}]),
  case StatusCode of
    200 ->
      ok;
    _ ->
      io:format("BOT set_commands | Error, status code: ~p~n", [StatusCode])
  end.

send_message(ChatId, Text) when is_list(Text) ->
  send_message(ChatId, list_to_binary(Text));
send_message(ChatId, Text) when is_binary(Text) ->
  RequestBody = #{<<"chat_id">> => ChatId, <<"text">> => Text},
  send_message(RequestBody).

send_message(Body) when is_map(Body) ->
  Request =
    {os:getenv("BASE_URL") ++ "sendMessage", [], "application/json", jsx:encode(Body)},
  io:format("BOT send_message | request: ~p~n", [Request]),
  {ok, {{_, StatusCode, _}, Headers, ResponseBody}} = httpc:request(post, Request, [], []),
  io:format("BOT send_message | response: ~p~n", [{ok, StatusCode, Headers, ResponseBody}]),
  case StatusCode of
    200 ->
      ok;
    _ ->
      io:format("BOT send_message | Error, status code: ~p~n", [StatusCode])
  end.

send_topics(ChatId) ->
  RequestBody =
    #{<<"chat_id">> => ChatId,
      <<"text">> => <<"Please select a topic.">>,
      <<"reply_markup">> =>
        #{<<"one_time_keyboard">> => true,
          <<"keyboard">> =>
            [[#{<<"text">> => <<"news">>},
              #{<<"text">> => <<"sport">>},
              #{<<"text">> => <<"tech">>},
              #{<<"text">> => <<"world">>}],
             [#{<<"text">> => <<"finance">>},
              #{<<"text">> => <<"politics">>},
              #{<<"text">> => <<"business">>},
              #{<<"text">> => <<"economics">>}],
             [#{<<"text">> => <<"entertainment">>},
              #{<<"text">> => <<"beauty">>},
              #{<<"text">> => <<"travel">>},
              #{<<"text">> => <<"music">>}],
             [#{<<"text">> => <<"food">>},
              #{<<"text">> => <<"science">>},
              #{<<"text">> => <<"gaming">>},
              #{<<"text">> => <<"energy">>}]]}},
  send_message(RequestBody).

init_request() ->
  inets:start(),
  ssl:start().
