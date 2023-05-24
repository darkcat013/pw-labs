-module(bot_request_handler).

-export([init/2]).

init(#{method := <<"POST">>} = Req, State) ->
  io:format("REQUEST HANDLER init | New request: ~n~p~n", [Req]),
  {ok, Body, NewReq} = cowboy_req:read_body(Req),
  BodyMap = jsx:decode(Body),
  io:format("REQUEST HANDLER init | Body: ~n~p~n", [BodyMap]),
  handle_body(NewReq, State, BodyMap);
init(#{method := Method} = Req, State) ->
  io:format("REQUEST HANDLER init | Invalid Method: ~p~nReq: ~p~nState~p~n",
            [Method, Req, State]),
  return_method_not_allowed(Req, State, iolist_to_binary(["Invalid Method: ", Method])).

handle_body(Req, State, #{<<"message">> := #{<<"text">> := Text}} = Body) ->
  TextString = binary_to_list(Text),
  ChatId = get_chat_id(Body),
  case db:get_last_action(ChatId) of
    [{ChatId, "/latest_news_topic"}] ->
      case lists:member(Text, db:get_topics()) of
        true ->
          handle_topic(ChatId, TextString);
        _ ->
          bot:send_message(ChatId, "Invalid topic")
      end;
    _ ->
      handle_command(Body, TextString)
  end,
  db:update_last_action(ChatId, TextString),
  {ok, Req, State};
handle_body(Req, State, Body) ->
  bot:send_message(get_chat_id(Body), "Invalid command"),
  {ok, Req, State}.

handle_command(Body, Text) ->
  {ChatId, FirstName} = get_chat_id_and_first_name(Body),
  case Text of
    "/start" ->
      bot:send_message(ChatId,
                       "Greetings " ++ FirstName ++ ". Use the menu to interract with the bot.");
    "/latest_news" ->
      handle_latest_news(ChatId);
    "/latest_news " ++ KeywordsString ->
      handle_latest_news(ChatId, KeywordsString);
    "/latest_news_topic" ->
      handle_topic(ChatId);
    "/save_news" ->
      bot:send_message(ChatId, "This command requires an URL");
    "/save_news " ++ Url ->
      db:save_link(ChatId, Url),
      bot:send_message(ChatId, "Success!");
    "/saved_news" ->
      handle_saved_news(ChatId);
    "/delete_saved" ->
      bot:send_message(ChatId, "This command requires an URL");
    "/delete_saved " ++ Url ->
      db:delete_link(ChatId, Url),
      bot:send_message(ChatId, "Success!");
    "/delete_all_saved" ->
      db:delete_all_links(ChatId),
      bot:send_message(ChatId, "Success!");
    _ ->
      bot:send_message(ChatId, "Invalid command")
  end,
  ok.

handle_latest_news(ChatId) ->
  case news_api:get_latest() of
    {ok, Response} ->
      bot:send_message(ChatId, Response);
    _ ->
      bot:send_message(ChatId, "An error occured when fetching the news, try again later.")
  end.

handle_latest_news(ChatId, Keywords) ->
  case news_api:get_latest(Keywords) of
    {ok, Response} ->
      bot:send_message(ChatId, Response);
    _ ->
      bot:send_message(ChatId, "An error occured when fetching the news, try again later.")
  end.

handle_topic(ChatId) ->
  bot:send_topics(ChatId).

handle_topic(ChatId, Topic) ->
  case news_api:get_latest_topic(Topic) of
    {ok, Response} ->
      bot:send_message(ChatId, Response);
    _ ->
      bot:send_message(ChatId, "An error occured when fetching the news, try again later.")
  end.

handle_saved_news(ChatId) ->
  case db:get_user_links(ChatId) of
    [{ChatId, []}] ->
      bot:send_message(ChatId, "Unfortunately for you, however, you are newsless.");
    [{ChatId, Links}] ->
      bot:send_message(ChatId, iolist_to_binary(["News\n", lists:join("\n\nNews\n", Links)]));
    _ ->
      bot:send_message(ChatId, "Unfortunately for you, however, you are newsless.")
  end.

get_chat_id(Body) ->
  #{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}}} = Body,
  ChatId.

get_chat_id_and_first_name(Body) ->
  #{<<"message">> :=
      #{<<"chat">> := #{<<"id">> := ChatId, <<"first_name">> := FirstName}}} =
    Body,
  {ChatId, binary_to_list(FirstName)}.

return_method_not_allowed(Req, State, Message) ->
  NewReq = cowboy_req:reply(405, #{<<"content-type">> => <<"text/plain">>}, Message, Req),
  {ok, NewReq, State}.
