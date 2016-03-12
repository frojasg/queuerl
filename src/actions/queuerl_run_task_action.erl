-module(queuerl_run_task_action).

-export([call/1]).

call(Task) ->
  NewTask = queuerl_task:increase_attempts(Task),
  {ok, Pid} = queuerl_worker_sup:start_child(NewTask),
  Pid.
