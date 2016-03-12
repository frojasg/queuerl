-module(queuerl_retry_task_action).

-export([call/2]).

-spec call(queuerl_task:task(), any()) -> {ok, queuerl_task:task()}
					  | {error, {run_out_of_retries, queuerl_task:task()}}.
call(Task, Error) ->
  erlang:display(Error), %use lagger?
  NewTask = queuerl_task:change_status(retrying, Task),
  MaxAttempts = queuerl_task:get_max_attempts(NewTask),
  Attempts = queuerl_task:get_attempts(NewTask),
  case Attempts >= MaxAttempts of
    true  -> handle_out_of_retries(NewTask);
    false -> handle_retry_task(NewTask)
  end.

handle_out_of_retries(Task) ->
  NewTask = queuerl_task:change_status(errored, Task),
  ErrorMsg = run_out_of_retries,
  {error, {ErrorMsg, NewTask}}.

handle_retry_task(Task) ->
  ok = queuerl:enqueue(Task),
  {ok, Task}.
