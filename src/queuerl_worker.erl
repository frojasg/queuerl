-module(queuerl_worker).

-export([run_task/1]).

run_task(Task) ->
  {ok, Pid} = queuerl_worker_sup:start_child(Task),
  Pid.
