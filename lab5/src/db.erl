-module(db).

-export([init/0, get_topics/0, update_last_action/2, get_last_action/1, get_user_links/1,
         save_link/2, delete_link/2, delete_all_links/1]).

init() ->
  ets:new(topics, [duplicate_bag, public, named_table]),
  ets:insert_new(topics,
                 {[<<"news">>,
                   <<"sport">>,
                   <<"tech">>,
                   <<"world">>,
                   <<"finance">>,
                   <<"politics">>,
                   <<"business">>,
                   <<"economics">>,
                   <<"entertainment">>,
                   <<"beauty">>,
                   <<"travel">>,
                   <<"music">>,
                   <<"food">>,
                   <<"science">>,
                   <<"gaming">>,
                   <<"energy">>]}),
  dets:open_file(users, [{file, "users.dets"}]),
  dets:open_file(user_last_action, [{file, "user_last_action.dets"}]),
  ok.

get_topics() ->
  ets:last(topics).

update_last_action(ChatId, LastAction) ->
  dets:insert(user_last_action, {ChatId, LastAction}).

get_last_action(ChatId) ->
  dets:lookup(user_last_action, ChatId).

get_user_links(ChatId) ->
  dets:lookup(users, ChatId).

save_link(ChatId, Link) ->
  case get_user_links(ChatId) of
    [{ChatId, Links}] ->
      dets:insert(users, {ChatId, Links ++ [Link]});
    _ ->
      dets:insert(users, {ChatId, [Link]})
  end.

delete_link(ChatId, Link) ->
  [{ChatId, Links}] = get_user_links(ChatId),
  dets:insert(users, {ChatId, lists:delete(Link, Links)}).

delete_all_links(ChatId) ->
  dets:insert(users, {ChatId, []}).
