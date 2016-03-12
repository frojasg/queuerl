-module(queuerl_notify_task_errored_action).

-export([call/2]).

call(Task, Error) ->
  Client = queuerl_task:get_client_pid(Task),
  TaskUuid = queuerl_task:get_uuid(Task),
  Client ! {task_errored, {TaskUuid, Error}}.
