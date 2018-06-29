%%%-------------------------------------------------------------------
%% @doc exercise public API
%% @end
%%%-------------------------------------------------------------------

-module(exercise_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, get_env/2, set_env/2]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    exercise_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

get_env(Parameter, Default) ->
    application:get_env(exercise, Parameter, Default).

set_env(Parameter, Value) ->
    application:set_env(exercise, Parameter, Value).
%%====================================================================
%% Internal functions
%%====================================================================
