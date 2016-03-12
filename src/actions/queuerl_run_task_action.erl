-module(queuerl_run_task_action).

-export([call/1]).

-spec call(queuerl_task:task()) -> queuerl_task:task().
call(Task) ->
  NewTask = queuerl_task:increase_attempts(Task),
  {ok, WorkerPid} = queuerl_worker_sup:start_child(NewTask),
  {ok, queuerl_task:set_worker_pid(WorkerPid, NewTask)}.

