-module(queuerl_enqueue_task_action).

-export([call/1]).

-spec call(queuerl_task:task()) -> ok.
call(Task) ->
  queuerl_controller:enqueue(Task),
  ok.
