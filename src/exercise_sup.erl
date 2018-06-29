%%%-------------------------------------------------------------------
%% @doc exercise top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(exercise_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    Database =
        {exercise_db,
         {exercise_db, start_link, []},
         permanent, 5000, worker, dynamic
        },
    Scheduler =
        {exercise_scheduler,
         {exercise_scheduler, start_link, []},
         permanent, 5000, worker, dynamic
        },
    HttpSrv =
        {exercise_http_server_api,
         {exercise_http_server_api, start_link, []},
         permanent, 5000, worker, dynamic
        },
    Children = [Database, Scheduler, HttpSrv],
    {ok, {{one_for_one, 5, 10}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
