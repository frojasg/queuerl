-module(queuerl_task).

-record(task, {function, status, uuid, attempts, max_attempts, worker_pid, client_pid}).
-type task_status() :: ready | complete | errored | retrying.
-opaque task() :: #task{function :: fun(),
			status :: task_status(),
			uuid :: uuid:uuid(),
			attempts :: integer(),
			max_attempts :: integer(),
			worker_pid :: pid(),
			client_pid :: pid()}.
-export_type([task/0]).

-export([new/2,
	 perform/1,
	 increase_attempts/1,
	 set_worker_pid/2,
	 get_worker_pid/1,
	 change_status/2,
	 get_max_attempts/1,
	 get_attempts/1,
	 get_client_pid/1,
	 get_uuid/1]).

-spec new(fun(), pid()) -> task().
new(Function, ClientPid) ->
  #task{function = Function,
	status = ready,
	uuid = uuid:uuid1(),
	attempts = 0,
	max_attempts = 6,
        client_pid = ClientPid}.

-spec perform(task()) -> none().
perform(#task{function = Fun}) ->
  Fun().

-spec increase_attempts(task()) -> task().
increase_attempts(#task{attempts = Value} = Task) ->
  Task#task{attempts = Value + 1}.

-spec set_worker_pid(pid(), task()) -> task().
set_worker_pid(Value, Task) ->
  Task#task{worker_pid = Value}.

-spec get_worker_pid(task()) -> pid().
get_worker_pid(#task{worker_pid = Value}) ->
  Value.

-spec change_status(task_status(), task()) -> task().
change_status(Status, Task) ->
  Task#task{status = Status}.

-spec get_max_attempts(task()) -> integer().
get_max_attempts(#task{max_attempts = Value}) ->
  Value.

-spec get_attempts(task()) -> integer().
get_attempts(#task{attempts = Value}) ->
  Value.

-spec get_client_pid(task()) -> pid().
get_client_pid(#task{client_pid = Value}) ->
  Value.

-spec get_uuid(task()) -> string().
get_uuid(#task{uuid = Value}) ->
  Value.
