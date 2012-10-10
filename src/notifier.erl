-module(notifier).

-export([start/0]).
-export([watchers_on_notifier/1, watcher_count/1]).
-export([user_watching/1, watcher_watching/1]).

-export([events_for_user/1, events_for_watcher/1]).

-export([event/2, event/3]).
-export([event/1]).
-export([remove_event_from_user/2]).

-export([user_watch/2, watcher_watch/2]).
-export([user_unwatch/2, watcher_unwatch/2]).

-import(proplists, [get_value/2]).

%%%--------------------------------------------------------------------
%%% starting
%%%--------------------------------------------------------------------
start() ->
  application:start(notifier).

%%%--------------------------------------------------------------------
%%% Notification Reading
%%%--------------------------------------------------------------------
watchers_on_notifier(Notifier) ->
  smembers(Notifier).

watcher_count(Notifier) ->
  scard(Notifier).

%%%--------------------------------------------------------------------
%%% Watcher Reading
%%%--------------------------------------------------------------------
events_for_user(Uid) ->
  events_for(ukey(Uid)).

events_for_watcher(WatcherId) ->
  events_for(wkey(WatcherId)).

events_for(WatcherId) ->
  zmembers(WatcherId).

% all events this user is watching
user_watching(Uid) ->
  watching(ukey(Uid)).

% all events this watcher is watchin
watcher_watching(WatcherId) ->
  watching(wkey(WatcherId)).

watching(Watcher) ->
  smembers(Watcher).

%%%--------------------------------------------------------------------
%%% Event Happening
%%%--------------------------------------------------------------------
event(Notifier, What) ->
  event(Notifier, What, epoch()).

event(Notifier, What, Timestamp) ->
  % when getting new event:
  % - generate new event ID
  % - log event as type with detauls
  % - check keys interested in this event
  % - notify keys interested in this event the event exists
  Watchers = watchers_on_notifier(Notifier),
  case Watchers of
    [] -> ok;  % if no watchers, just pass through.
     _ -> EventId = integer_to_list(incr(event_counter), 36),
          hmset(event, EventId, [ts, Timestamp,
                                 what, term_to_binary(What, [compressed])]),
          % this should be in an async queue system (?):
          notify(Watchers, EventId, Timestamp)
  end.

%%%--------------------------------------------------------------------
%%% Event Clearing From User
%%%--------------------------------------------------------------------
remove_event_from_user(EventId, Uid) ->
  zrem(ukey(Uid), EventId).

%%%--------------------------------------------------------------------
%%% Event Reading
%%%--------------------------------------------------------------------
event(EventId) ->
  E = hgetall(event, EventId),
  Thing = get_value(what, E),
  lists:keyreplace(what, 1, E, {what, binary_to_term(Thing)}).

%%%--------------------------------------------------------------------
%%% Watcher Updating
%%%--------------------------------------------------------------------
notify(Watchers, Event, Timestamp) ->
  notify(Watchers, Event, Timestamp, []).

notify([Watcher = <<"uid:", _/binary>> | T], EventId, Timestamp, Seen) ->
  zadd(Watcher, Timestamp, EventId),
  notify(T, EventId, Timestamp, Seen);
notify([Watcher = <<"notifier:", Notifier/binary>> | T],
    EventId, Timestamp, Seen) ->
  case lists:member(Watcher, Seen) of
    false -> % non-cyclical hierarchical notifications.  dag, yo.
             % we give watchers the non-namespaced prefix because
             % it has its own namespacing issues.
             Watchers = watchers_on_notifier(Notifier),
             notify(Watchers, EventId, Timestamp),
             % only log potentially recursive watchers:
             notify(T, EventId, Timestamp, [Watcher | Seen]);
     true -> notify(T, EventId, Timestamp, Seen)
  end;
notify([], EventId, Timestamp, Seen) ->
  {EventId, Timestamp, Seen}.

% enroll Uid as a watcher for Notifier
user_watch(Notifier, Uid) ->
  watch(Notifier, ukey(Uid)).

% enroll Watcher as a watcher for Notifier
watcher_watch(Notifier, WatcherId) ->
  watch(Notifier, wkey(WatcherId)).

watch(Notifier, Watcher) ->
  % add to Notifier and add to set of things this user/watcher is watching
  sadd(Watcher, Notifier),
  sadd(Notifier, Watcher).

%%%--------------------------------------------------------------------
%%% Watcher Removal
%%%--------------------------------------------------------------------
% de-enroll Uid as a watcher for Notifier
user_unwatch(Notifier, Uid) ->
  unwatch(Notifier, ukey(Uid)).

% de-enroll Watcher as a watcher for Notifier
watcher_unwatch(Notifier, WatcherId) ->
  unwatch(Notifier, wkey(WatcherId)).

unwatch(Notifier, Watcher) ->
  srem(Watcher, Notifier),
  srem(Notifier, Watcher).

%%%--------------------------------------------------------------------
%%% Key Doers
%%%--------------------------------------------------------------------
-compile({inline, [{nkey, 1}, {nkey, 2},
                   {ukey, 1}, {wkey, 1},
                   {sadd, 2}, {srem, 2}, {smembers, 1}, {scard, 1},
                   {hmset, 3}, {hgetall, 2},
                   {incr, 1},
                   {zadd, 3}, {zrem, 2}, {zmembers, 1}]}).

nkey(What) ->
  eru:er_key(notify, What).

nkey(Namespace, What) ->
  nkey(eru:er_key(Namespace, What)).

% watchers are <<"uid:UID">> or <<"notifier:WatcherId">>
ukey(Uid) ->
  eru:er_key(uid, Uid).
wkey(WatcherId) ->
  eru:er_key(notifier, WatcherId).

%%%--------------------------------------------------------------------
%%% Redis Doers
%%%--------------------------------------------------------------------
sadd(Key, What) ->
  er:sadd(redis_notifier, nkey(Key), What).

srem(Key, What) ->
  er:srem(redis_notifier, nkey(Key), What).

smembers(Key) ->
  er:smembers(redis_notifier, nkey(Key)).

scard(Key) ->
  er:scard(redis_notifier, nkey(Key)).

hmset(Namespace, Key, Whats) ->
  er:hmset(redis_notifier, nkey(Namespace, Key), Whats).

hgetall(Namespace, Key) ->
  er:hgetall_p(redis_notifier, nkey(Namespace, Key)).

incr(Key) ->
  er:incr(redis_notifier, nkey(Key)).

% Note: sorted sets here are *ONLY* for storing watcher events.
zadd(Key, Score, What) ->
  er:zadd(redis_notifier, nkey(Key, events), Score, What).

zrem(Key, What) ->
  er:zrem(redis_notifier, nkey(Key, events), What).

zmembers(Key) ->
  er:zrevrange(redis_notifier, nkey(Key, events), 0, -1).

-compile({inline, epoch/0}).
epoch() ->
  {Mega, Sec, _} = now(),
  Mega * 1000000 + Sec.
