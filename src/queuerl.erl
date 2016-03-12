-module(queuerl).

-export([new_task/1, enqueue/1]).

new_task(Name) ->
  queuerl_task:new(Name).

enqueue(Task) ->
  queuerl_controller:enqueue(Task).
