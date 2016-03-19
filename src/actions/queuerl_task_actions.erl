%%% Actions that can be done with a Task.
-module(queuerl_task_actions).

%%% API declaration
-export([enqueue/1,
	 run/1,
	 retry/2,
	 notify_client/2]).

%%% API implementation
-spec enqueue(queuerl_tasks:task()) -> ok.
enqueue(Task) ->
  queuerl_controller:enqueue(Task),
  ok.

-spec run(queuerl_tasks:task()) -> queuerl_tasks:task().
run(Task) ->
  NewTask = queuerl_tasks:increase_attempts(Task),
  {ok, WorkerPid} = queuerl_worker_sup:start_child(NewTask),
  {ok, queuerl_tasks:set_worker_pid(WorkerPid, NewTask)}.

-spec retry(queuerl_task:task(), any()) -> {ok, queuerl_task:task()}
					  | {error, {queuerl_task:task(), run_out_of_retries}}.
retry(Task, Error) ->
  erlang:display(Error), %use lagger?
  NewTask = queuerl_tasks:change_status(retrying, Task),
  MaxAttempts = queuerl_tasks:get_max_attempts(NewTask),
  Attempts = queuerl_tasks:get_attempts(NewTask),
  case Attempts >= MaxAttempts of
    true  -> handle_out_of_retries(NewTask);
    false -> handle_retry_task(NewTask)
  end.

-spec notify_client(queuerl_tasks:task(), any()) -> any().
notify_client(Task, Info) ->
  Client = queuerl_tasks:get_client_pid(Task),
  TaskUuid = queuerl_tasks:get_uuid(Task),
  NotificationType = notification_type(Info),
  Client ! {NotificationType, {TaskUuid, Info}}.

%%% Internal functions
notification_type({succeeded}) ->
  succeeded;
notification_type({errored, _}) ->
  errored.

handle_out_of_retries(Task) ->
  ErrorMsg = run_out_of_retries,
  {error, {Task, ErrorMsg}}.

handle_retry_task(Task) ->
  ok = queuerl:enqueue(Task),
  {ok, Task}.

