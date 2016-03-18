-module(queuerl).

-export([new_task/1,
	 enqueue/1,
	 get_task/1]).

new_task(Function) ->
  queuerl_tasks:new(Function, self()).

enqueue(Task) ->
  queuerl_task_actions:enqueue(Task).

get_task(TaskUuid) ->
  queuerl_controller:get_task(TaskUuid).
