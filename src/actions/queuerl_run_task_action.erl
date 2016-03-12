-module(queuerl_run_task_action).

-export([call/1]).

-spec call(queuerl_task:task()) -> queuerl_task:task().
call(Task) ->
  NewTask = queuerl_task:increase_attempts(Task),
  WorkerPid = queuerl_worker:run_task(NewTask),
  {ok, queuerl_task:set_worker_pid(WorkerPid, NewTask)}.

