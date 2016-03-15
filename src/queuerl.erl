-module(queuerl).

-export([new_task/1,
	 enqueue/1,
	 run/1,
	 retry/2,
	 notify_client/2,
	 get_task/1]).

new_task(Function) ->
  queuerl_tasks:new(Function, self()).

enqueue(Task) ->
  queuerl_task_actions:enqueue(Task).

run(Task) ->
  queuerl_task_actions:run(Task).

retry(Task, RetryInfo) ->
  queuerl_task_actions:retry(Task, RetryInfo).

notify_client(Task, Info) ->
  queuerl_task_actions:notify_client(Task, Info).

get_task(TaskUuid) ->
  queuerl_controller:get_task(TaskUuid).
