notifier: erlang and redis-backed hierarchical user alerting/notification system
================================================================================

Status
------
`notifier` is a generic notifier/feed system of unknown scalability.

All events are stored in redis.

You ask notifier to listen to hierarchical keys.
When events happen, events get added
to your event queue.  You can read/delete any items in your queue at
any time.

EXAMPLE TIME: If you trigger an event on a key `users.signups.thursday`, anybody
directly listening to that key, or listening to `users.signups` or
just `users` gets the event sent to their event queue.

Usage
-----
See the tests for basic usage examples.  The code below is extracted from
production usage of `notifier`:

    % We can spawn this if necessary:
    tell(Event, Body) when is_list(Event) andalso is_tuple(Body) ->
      notifier:event(ekey(Event), Body).

    % Create Event Notifier Key
    ekey(Parts) when is_list(Parts) ->
      iolist_to_binary(string:join(eru:ll(Parts), ".")). % where eru:ll/1 turns a list of anything into a list of strings (ll: list-ize list).

    listen(Event, Uid) when is_list(Event) ->
      notifier:user_watch(ekey(Event), Uid).

    events_for_user(Uid) ->
      EventIds = notifier:events_for_user(Uid, 100),  % cap at 100 for now.
      [event_format(EventId) || EventId <- EventIds].

    event_format(EventId) ->
      Event = notifier:event(EventId),
      {What, Desc} = describe_event(val(e, Event)),
      [{ts, fixtime(list_to_integer(binary_to_list(val(ts, Event))))},
       {by, val(by, Event)},
       {id, EventId},
       {what, What},
       {desc, Desc}].

    fixtime(Epoch) when is_integer(Epoch) ->
      % the magic number is greg seconds of jan 1 1970.
      iso8601:format(calendar:gregorian_seconds_to_datetime(62167219200 + Epoch)).

    event_clear(Uid, EventId) ->
      notifier:remove_event_from_user(EventId, Uid).

    describe_event({signup, Uid}) ->
      {<<"signup">>, iof("~s (uid ~s) signed up", [lu(Uid), Uid])};
    describe_event({cancel, Uid, NowOrLater}) ->
      {<<"canceled subscription">>,
       iof("~s (uid ~s) canceled subscription ~p",
        [lu(Uid), Uid, NowOrLater])};
    describe_event({switch, Uid, CurrentPlan, NewPlan}) ->
      {<<"plan switch">>, iof("~s (uid ~s) switched from plan ~p to plan ~p",
        [lu(Uid), Uid, CurrentPlan, NewPlan])};
    describe_event({comment, Uid, Root, ThreadId, CommentId}) ->
      {<<"comment">>, iof("~s commented on ~s",
       [lu(Uid), lc(Root, ThreadId, CommentId)])}.

    lu(Uid) ->
      Username = username(Uid),
      io_lib:format("<a href=\"/profile/~s\" class=\"username\">~s</a>",
       [Username, Username]).

Examples of things you could use as a first argument to `tell/2` above:
  - `[signup, Plan]`
  - `[signup]`
  - `[subscribe, cancel]`
  - `[switching]`
  - `[accessLevel, Type, Id, NewAccessLevel]`
  - `[comment, repliedTo, uid, ParentUid]`
  - `[comment, postedTo, RootId]`
  - `[comment, repliedTo, thread, ThreadId]`
  - `[comment, postedBy, Uid]`
  - `[voted, onUid, CommentOwnerUid]`
  - `[voted, Direction, onUid, CommentOwnerUid]`
  - `[voted, byUid, Uid]`
  - `[voted, Direction, byUid, Uid]`
  - `[voted, RootId, ParentId, ChildId]`
  - `[voted, Direction, RootId, ParentId, ChildId]`

Each event has an event key *and* a data payload.  e.g. the `[switching]`
key would be triggered by: `tell([switching], {switch, Uid, CurrentPlan, NewPlan})`

A longer example is triggering on comment voting keys:

      spawn(fun() ->
        EventBody = {vote, Uid, Direction, RootId, ParentId, ChildId, UseWeight},
        tell([voted, onUid, CommentOwnerUid], EventBody),
        tell([voted, byUid, Uid], EventBody),
        tell([voted, RootId, ParentId, ChildId], EventBody),
        tell([voted, Direction, onUid, CommentOwnerUid], EventBody),
        tell([voted, Direction, byUid, Uid], EventBody),
        tell([voted, Direction, RootId, ParentId, ChildId], EventBody)
      end).


Building
--------
        rebar get-deps
        rebar compile

Testing
-------
        rebar eunit skip_deps=true suite=weighter

Next Steps
----------
Everything and nothing.  Keep it simple.
