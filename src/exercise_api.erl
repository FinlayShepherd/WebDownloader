%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017
%%% @doc
%%% API module for exercise
%%% @end
%%% Created : 25. Jul 2017 10:15 AM
%%%-------------------------------------------------------------------
-module(exercise_api).
-author("finlay.shepherd").

%% API
-export([download/1,
         get/1,
         search_name/1,
         search_address/1,
         search_text/2,
         search_text_all/1,
         get_errors/0]).


%%==============================================================================
%% API functions
%%==============================================================================

%%
%% Schedules a list or single web page to be downloaded.
%%
-spec download(Jobs :: [{binary(), binary()}]|{Name :: binary(), HttpAddress :: binary()}) -> ok.
download({Name, HttpAddress}) ->
    exercise_scheduler:download([{Name, HttpAddress}]);
download(Jobs) ->
    exercise_scheduler:download(Jobs).

%%
%% Retrieves a downloaded page's content from the database
%%
-spec get(Key :: {binary(), binary()}) -> {ok, Data :: binary()}|{error, not_found}.
get(Key) ->
    exercise_db:get(Key).

%%
%% Searches the key of the downloaded pages for a similar name
%%
-spec search_name(Name :: binary()) -> [Key :: {binary(), binary()}].
search_name(Name) ->
    exercise_search:search_by_name(Name).

%%
%% Searches the key of the downloaded pages for a similar http address
%%
-spec search_address(Address :: binary()) -> [Key :: {binary(), binary()}].
search_address(Address) ->
    exercise_search:search_by_http_address(Address).

%%
%% Searches the content of a specified page in the database for a string
%%
-spec search_text(Key :: {binary(), binary()}, Text :: binary()) ->
    [{Key :: {binary(), binary()}, Line :: integer(), Text :: binary()}].
search_text(Key, Text) ->
    exercise_search:search_page(Key, Text).

%%
%% Searches the content of all stored pages in the database for a string
%%
-spec search_text_all(Text :: binary()) ->
    [[{Key :: {binary(), binary()}, Line :: integer(), Text :: binary()}]].
search_text_all(Text) ->
    exercise_search:search_all_pages(Text).

-spec get_errors() ->
    [{Name :: binary(), Address :: binary(), Error :: term()}].
get_errors() ->
    exercise_scheduler:get_errors().