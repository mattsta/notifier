-module(notifier_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

supname(Name) ->
  list_to_atom(atom_to_list(Name) ++ "_sup").

er_sup(Name, IP, Port) ->
  SupName = supname(Name),
  {SupName,
   {er_pool, start_link, [Name, IP, Port]},
    permanent, 5000, worker, [er_pool]}.

init([]) ->
  RedisNotifier = er_sup(redis_notifier, "127.0.0.1", 6383),

  Processes = [RedisNotifier],

  Strategy = {one_for_one, 10, 10},
  {ok,
   {Strategy, lists:flatten(Processes)}}.
