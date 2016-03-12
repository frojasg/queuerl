-module(queuerl_task).

-record(task, {function, status, uuid, attempts}).
-opaque task() :: #task{function :: fun(),
			status :: ready | complete | errored,
			uuid :: uuid:uuid(),
			attempts :: integer()}.
-export_type([task/0]).

-export([new/1, perform/1, increase_attempts/1]).

-spec new(fun()) -> task().
new(Function) ->
  #task{function = Function,
	status = ready,
	uuid = uuid:uuid1(),
	attempts = 0}.

-spec perform(task()) -> none().
perform(#task{function = Fun}) ->
  Fun().

-spec increase_attempts(task()) -> task().
increase_attempts(#task{attempts = Value} = Task) ->
  Task#task{attempts = Value + 1}.
