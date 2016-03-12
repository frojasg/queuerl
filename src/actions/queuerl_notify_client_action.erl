-module(queuerl_notify_client_action).

-export([call/2]).

call(Task, Info) ->
  Client = queuerl_task:get_client_pid(Task),
  TaskUuid = queuerl_task:get_uuid(Task),
  NotificationType = notification_type(Info),
  Client ! {NotificationType, {TaskUuid, Info}}.

notification_type({succeeded}) ->
  succeeded;
notification_type({errored, _}) ->
  errored.
