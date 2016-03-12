-module(queuerl_retry_task_action).

-export([call/2]).

-spec call(queuerl_task:task(), any()) -> ok
					  | {error, {run_out_of_retries, queuerl_task:task()}}.
call(Task, Error) ->
  erlang:display(Error), %use lagger?
  NewTask = queuerl_task:change_status(retrying, Task),
  MaxAttempts = queuerl_task:get_max_attempts(NewTask),
  Attempts = queuerl_task:get_attempts(NewTask),
  case Attempts >= MaxAttempts of
    true  -> handle_out_of_retries(NewTask);
    false -> queuerl:enqueue(NewTask)
  end.

handle_out_of_retries(Task) ->
  NewTask = queuerl_task:change_status(errored, Task),
  ErrorMsg = run_out_of_retries,
  {error, {ErrorMsg, NewTask}}.
