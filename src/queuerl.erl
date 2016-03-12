-module(queuerl).

-export([new_task/1,
	 enqueue/1,
	 run/1,
	 retry/2,
	 notify_client/2,
	 get_task/1]).

new_task(Function) ->
  queuerl_task:new(Function, self()).

enqueue(Task) ->
  queuerl_enqueue_task_action:call(Task).

run(Task) ->
  queuerl_run_task_action:call(Task).

retry(Task, RetryInfo) ->
  queuerl_retry_task_action:call(Task, RetryInfo).

notify_client(Task, Info) ->
  queuerl_notify_client_action:call(Task, Info).

get_task(TaskUuid) ->
  queuerl_controller:get_task(TaskUuid).
