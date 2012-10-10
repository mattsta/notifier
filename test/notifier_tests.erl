-module(notifier_tests).

-include_lib("eunit/include/eunit.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
notifier_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Sign up for reply notification",
       fun reply_signup/0},
     {"Trigger reply notification",
       fun reply/0},
     {"Read reply notification then remove event",
       fun read_reply_event/0},

     {"Sign up for messaging notification",
       fun messaging_signup/0},
     {"Add messaging notification to reply notification",
       fun add_to_reply/0},
     {"Send message and check notification appears for user",
       fun message_and_check/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
reply_signup() ->
  notifier:user_watch("comment.reply.BOB", bob).

reply() ->
  notifier:event("comment.reply.BOB", 6, {reply, clo, 16}).

read_reply_event() ->
  Events = notifier:events_for_user(bob),
  [EventId] = Events,
  Contents = notifier:event(EventId),
  notifier:remove_event_from_user(EventId, bob),
  ZeroEvents = notifier:events_for_user(bob),
  ?assertEqual(<<"1">>, EventId),
  ?assertEqual([{ts, <<"6">>}, {what, {reply, clo, 16}}], Contents),
  ?assertEqual([], ZeroEvents).

messaging_signup() ->
  notifier:user_watch("comment-catcher", bob).

add_to_reply() ->
  notifier:watcher_watch("comment.reply-generic", "comment-catcher").

message_and_check() ->
  % trigger the event on reply-generic, but it should bubble up to
  % bob's subscription of comment-catcher because comment-catcher
  % is subscribed to comment.reply-generic.  got it?
  notifier:event("comment.reply-generic", 12, {reply, hier}),
  Events = notifier:events_for_user(bob),
  [EventId] = Events,
  Contents = notifier:event(EventId),
  ?assertEqual(<<"2">>, EventId),
  ?assertEqual([{ts, <<"12">>}, {what, {reply, hier}}], Contents).

%%%----------------------------------------------------------------------
%%% Set it up, tear it down
%%%----------------------------------------------------------------------
setup() ->
  application:start(er),
  er_pool:start_link(redis_notifier, "127.0.0.1", 6389),
  er:flushall(redis_notifier).

teardown(_) ->
  application:stop(er).
