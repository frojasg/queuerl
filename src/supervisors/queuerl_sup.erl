-module(queuerl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  SupFlags = #{strategy => one_for_all,
	       intensity => 1,
	       period => 5},

  Controller = #{id => controller,
		 start => {queuerl_controller, start_link, []},
		 restart => permanent,
		 shutdown => 5000,
		 type => worker,
		 modules => [queuerl_controller]},
  WorkerSup = #{id => worker_sup,
		start => {queuerl_worker_sup, start_link, []},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [queuerl_worker_sup]},
  {ok, {SupFlags, [Controller, WorkerSup]}}.
