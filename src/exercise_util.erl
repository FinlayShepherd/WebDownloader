%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Utility module
%%% @end
%%% Created : 21. Jul 2017 4:41 PM
%%%-------------------------------------------------------------------
-module(exercise_util).
-author("finlay.shepherd").

%% API
-export([to_string/1]).

%%--------------------------------------------------------------------
%% @doc
%% Takes a parameter and confirms if it's type is appropriate
%% @param Data that will later be used in HTTP request
%%
%% @return Data in string form
%% @end
%%--------------------------------------------------------------------
to_string(Data) when is_binary(Data) ->
    binary:bin_to_list(Data);
to_string(Data) when is_list(Data) ->
    Data.

