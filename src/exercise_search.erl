%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Module to search the key value store.
%%% @end
%%% Created : 21. Jul 2017 1:43 PM
%%%-------------------------------------------------------------------
-module(exercise_search).
-author("finlay.shepherd").

%% API
-export([search_by_name/1,
         search_by_http_address/1,
         search_page/2,
         search_all_pages/1]).

%%--------------------------------------------------------------------
%% @doc
%% Search a web page by part of stored name
%% @param Name, the name being searched for
%%
%% @return A list of all keys containing the search string in the name field
%% @end
%%--------------------------------------------------------------------
-spec search_by_name(Name :: binary()|string()) ->
    list([Key :: {binary(), binary()}]).
search_by_name(Name) ->
    Keys = exercise_db:get_all_keys(),
    search_for_names(Keys, Name, []).

%%--------------------------------------------------------------------
%% @doc
%% Search a web page by part of the http address
%% @param Address, the address being searched for
%%
%% @return A list of all keys containing the search string in the address field
%% @end
%%--------------------------------------------------------------------
-spec search_by_http_address(Address :: binary()) ->
    list([Key :: {binary(), binary()}]).
search_by_http_address(Address) ->
    Keys = exercise_db:get_all_keys(),
    search_for_address(Keys, Address, []).

%%--------------------------------------------------------------------
%% @doc
%% Search text inside of named web page
%% @param Key the value that the content of the page is stored at in the database
%% @param SearchValue the value to search for in the page
%%
%% @return A list of tuples containing the Key, the line a match was found at, and the match.
%% @end
%%--------------------------------------------------------------------
-spec search_page(Key :: {binary(), binary()}, SearchValue :: term()) ->
    list([{Key :: {binary(), binary()}, Line :: integer(), Symbol :: term()}]) | {error, invalid_key}.
search_page(Key, SearchValue) ->
    case exercise_db:get(Key) of
        {error, not_found} ->
            {error, invalid_key};
        {ok, PageContent} ->
            ContentList = binary:split(PageContent, <<"\n">>, [global]),
            search_page_list(ContentList, SearchValue, 0, Key, [])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Search text by all stored web pages
%% @param SearchValue the value to search for in the pages
%%
%% @return A list of lists of tuples containing the Key, the line a match was found at, and the match.
%% @end
%%--------------------------------------------------------------------
-spec search_all_pages(SearchValue :: term()) ->
    list([[{Key :: {binary(), binary()}, Line :: integer(), Symbol :: term()}]]).
search_all_pages(SearchValue) ->
    Keys = exercise_db:get_all_keys(),
    iterate_through_keys(Keys, SearchValue, []).

%%--------------------------------------------------------------------
%% @doc
%% Iterates through list of keys and checks name against the search value for a match
%% @param List of keys
%% @param The value to search for
%% @param Accumulator to store result
%%
%% @return A list of tuples containing the Key of any pages that had a match.
%% @end
%%--------------------------------------------------------------------
search_for_names([], _, Accumulator) ->
    Accumulator;
search_for_names([{Name, Address} | OtherKeys], SearchValue, Accumulator) ->
    case binary:match(Name, SearchValue) of
        nomatch ->
            search_for_names(OtherKeys, SearchValue, Accumulator);
        _ ->
            search_for_names(OtherKeys, SearchValue, [{Name, Address} | Accumulator])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Iterates through list of keys and checks Address against the search value for a match
%% @param List of keys
%% @param The value to search for
%% @param Accumulator to store result
%%
%% @return A list of tuples containing the Key of any pages that had a match.
%% @end
%%--------------------------------------------------------------------
search_for_address([], _, Accumulator) ->
    Accumulator;
search_for_address([{Name, Address} | OtherKeys], SearchValue, Accumulator) ->
    case binary:match(Address, SearchValue) of
        nomatch ->
            search_for_address(OtherKeys, SearchValue, Accumulator);
        _ ->
            search_for_address(OtherKeys, SearchValue, [{Name, Address} | Accumulator])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Iterates through list of lines in a page checking if each line contains the search value
%% @param List of page lines
%% @param The value to search for
%% @param Accumulator to store result
%%
%% @return A list of tuples containing the Key of the page, the line the page is on, and the search value.
%% @end
%%--------------------------------------------------------------------
search_page_list([], _, _, _, Accumulator) ->
    Accumulator;
search_page_list([Line | OtherLines], SearchValue, Counter, Key, Accumulator) ->
    case binary:match(Line, SearchValue) of
        nomatch ->
            search_page_list(OtherLines, SearchValue, Counter + 1, Key, Accumulator);
        _ ->
            search_page_list(OtherLines, SearchValue, Counter + 1, Key, [{Key, Counter, SearchValue} | Accumulator])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Iterates through list of pages stored in db and searches each page's content for the search string
%% @param List of pages
%% @param The value to search for
%% @param Accumulator to store result
%%
%% @return A list of lists of tuples containing the Key of the page, the line the page is on, and the search value.
%% @end
%%--------------------------------------------------------------------
iterate_through_keys([], _, Accumulator) ->
    Accumulator;
iterate_through_keys([Key | OtherKeys], SearchValue, Accumulator) ->
    Result = search_page(Key, SearchValue),
    iterate_through_keys(OtherKeys, SearchValue, [Result | Accumulator]).
