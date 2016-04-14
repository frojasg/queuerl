-module(queuerl_tests).

-include_lib("eunit/include/eunit.hrl").

queuerl_test_() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun (Status) ->
      [
        pool_startup(Status),
        is_alive(Status)
      ]
    end
  }.

start() ->
  % ?debugHere,
  {ok, Sup} = supervisor:start_link(queuerl_sup, []),
  [{ola, chao}, {sup, Sup}].

stop(_Status) ->
  %Sup = proplists:get_value(sup, Status),
  %exit(Sup, shutdown),
  ok.

pool_startup(_Status) ->
  ?_assert(true).

is_alive(Status) ->
  Sup = proplists:get_value(sup, Status),
  % ?debugVal(Sup),
  ?_assert(is_process_alive(Sup)).
