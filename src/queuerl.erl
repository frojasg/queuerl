-module(queuerl).

-export([new_task/1,
	 enqueue/1,
	 get_task/1]).

new_task(Function) ->
  queuerl_task:new(Function, self()).

enqueue(Task) ->
  queuerl_controller:enqueue(Task).

get_task(TaskUuid) ->
  queuerl_controller:get_task(TaskUuid).
