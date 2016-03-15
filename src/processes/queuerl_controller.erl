-module(queuerl_controller).

-behaviour(gen_server).

%% API
-export([start_link/0, enqueue/1, get_task/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {refs :: #{}, tasks :: #{}}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

enqueue(Task) ->
  gen_server:cast(?SERVER, {enqueue, Task}).

get_task(TaskUuid) ->
  gen_server:call(?SERVER, {task_status, TaskUuid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{refs = maps:new(),
	      tasks = maps:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({task_status, Uuid}, _From, #state{tasks = Tasks} = State) ->
  Reply = maps:get(Uuid, Tasks, undefined),
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec handle_cast({enqueue, queuerl_tasks:task()}, #state{}) -> any().
handle_cast({enqueue, Task}, #state{refs = Refs, tasks = Tasks} = State) ->
  {ok, NewTask} = queuerl:run(Task),
  TaskUuid = queuerl_tasks:get_uuid(NewTask),
  MonitorRef = monitor_task(NewTask),
  NewRefs = maps:put(MonitorRef, NewTask, Refs),
  NewTasks = maps:put(TaskUuid, NewTask, Tasks),
  {noreply, State#state{refs = NewRefs, tasks = NewTasks}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, _, Info}, State) ->
  handle_worker_down(Ref, Info, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec monitor_task(queuerl_tasks:task()) -> reference().
monitor_task(Task) ->
  WorkerPid = queuerl_tasks:get_worker_pid(Task),
  erlang:monitor(process, WorkerPid).

handle_worker_down(MonitorRef, Info, #state{refs = Refs, tasks = Tasks} = State) ->
  Task = maps:get(MonitorRef, Refs),
  TaskUuid = queuerl_tasks:get_uuid(Task),
  NewTask = handle_worker_down(Info, Task),
  NewTasks = maps:put(TaskUuid, NewTask, Tasks),
  {noreply, State#state{tasks = NewTasks}}.

handle_worker_down(normal, Task) ->
  NewTask = queuerl_tasks:change_status(succeeded, Task),
  queuerl:notify_client(NewTask, {succeeded}),
  NewTask;
handle_worker_down(Info, Task) ->
  Result = queuerl:retry(Task, Info),
  handle_retry_task(Result).

handle_retry_task({error, {Task, ErrorMsg}}) ->
  ErroredTask = queuerl_tasks:change_status(errored, Task),
  queuerl:notify_client(ErroredTask, {errored, ErrorMsg}),
  ErroredTask;
handle_retry_task({ok, Task}) ->
  Task.

