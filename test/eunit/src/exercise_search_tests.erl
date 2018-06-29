%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% exercise_search unit tests
%%% @end
%%% Created : 24. Jul 2017 10:42 AM
%%%-------------------------------------------------------------------
-module(exercise_search_tests).
-author("finlay.shepherd").

-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok.

all_test_() ->
    {foreach, fun setup/0,
        fun(_) ->
            meck:unload()
        end, [{"Searching for a name", fun search_name/0},
              {"Searching for a http address", fun search_address/0},
              {"Search for content in a page", fun search_page/0},
              {"Invalid key for search page", fun search_page_fail/0},
              {"Search all pages for field", fun search_all_pages/0}]}.

search_name() ->
    meck:expect(exercise_db, get_all_keys,
                fun() ->
                    [{<<"google">>, <<"http://www.google.com">>}, {<<"erlang">>, <<"http://www.erlang.org">>}] end),
    ?assertEqual([{<<"google">>, <<"http://www.google.com">>}], exercise_search:search_by_name(<<"goo">>)).

search_address() ->
    meck:expect(exercise_db, get_all_keys,
                fun() ->
                    [{<<"google">>, <<"http://www.google.com">>}, {<<"erlang">>, <<"http://www.erlang.org">>}] end),
    ?assertEqual([{<<"google">>, <<"http://www.google.com">>}], exercise_search:search_by_http_address(<<"google">>)).

search_page() ->
    meck:expect(exercise_db, get,
                fun({<<"erlang">>, <<"http://www.erlang.org">>}) ->
                    {ok, <<"PageContent\nPageContent\nname\nPageContent">>} end),
    ?assertEqual(
        [{{<<"erlang">>, <<"http://www.erlang.org">>}, 3, <<"PageContent">>},
         {{<<"erlang">>, <<"http://www.erlang.org">>}, 1, <<"PageContent">>},
         {{<<"erlang">>, <<"http://www.erlang.org">>}, 0, <<"PageContent">>}],
        exercise_search:search_page({<<"erlang">>, <<"http://www.erlang.org">>}, <<"PageContent">>)).

search_page_fail() ->
    meck:expect(exercise_db, get, fun({<<"erlang">>, <<"http://www.er">>}) -> {error, not_found} end),
    ?assertEqual({error, invalid_key},
                 exercise_search:search_page({<<"erlang">>, <<"http://www.er">>}, <<"PageContent">>)).

search_all_pages() ->
    meck:expect(exercise_db, get_all_keys,
                fun() ->
                    [{<<"google">>, <<"http://www.google.com">>}, {<<"erlang">>, <<"http://www.erlang.org">>}] end),
    meck:expect(exercise_db, get,
                fun({<<"erlang">>, <<"http://www.erlang.org">>}) ->
                    {ok, <<"PageContent\nPageContent\nname\nPageContent">>};
                   ({<<"google">>, <<"http://www.google.com">>}) ->
                       {ok, <<"PageContent\nname\nname\nPageContent">>} end),
    ?assertEqual(
        [[{{<<"erlang">>, <<"http://www.erlang.org">>}, 2, <<"name">>}],
         [{{<<"google">>, <<"http://www.google.com">>}, 2, <<"name">>},
          {{<<"google">>, <<"http://www.google.com">>}, 1, <<"name">>}]],
        exercise_search:search_all_pages(<<"name">>)).

