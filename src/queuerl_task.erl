-module(queuerl_task).

-record(task, {function, status, uuid}).
-opaque queuerl_task() :: #task{function :: fun(),
				status :: ready | complete | errored,
				uuid :: uuid:uuid()}.
-export_type([queuerl_task/0]).

-export([new/1]).

-spec new(string()) -> queuerl_task().
new(Name) ->
  #task{function = fun() -> true end,
	status = Name,
	uuid = uuid:uuid1()}.
