%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <Alert Logic>
%%% @doc
%%% Unit tests for exercise_api
%%% @end
%%% Created : 25. Jul 2017 2:29 PM
%%%-------------------------------------------------------------------
-module(exercise_api_tests).
-author("finlay.shepherd").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {
        foreach,
        fun() -> meck:new([exercise_scheduler, exercise_db, exercise_search]) end,
        fun(_) -> meck:unload() end,
        [
            {"Download test", fun schedule_download/0},
            {"Get test", fun get/0},
            {"Search for name test", fun search_name/0},
            {"Search for address test", fun search_address/0},
            {"Search specified page", fun search_text_in_page/0},
            {"Search all pages", fun search_all_stored_pages/0},
            {"Get errors", fun get_errors/0}
        ]
    }.

schedule_download() ->
    meck:expect(exercise_scheduler, download, 1, ok),
    Jobs = [{<<"erlang">>, <<"http://www.erlang.org">>}],
    ?assertEqual(ok, exercise_api:download(Jobs)).

get() ->
    meck:expect(exercise_db, get, [{<<"erlang">>, <<"http://www.erlang.org">>}], {ok, <<"data">>}),
    ?assertEqual({ok, <<"data">>}, exercise_api:get({<<"erlang">>, <<"http://www.erlang.org">>})).

search_name() ->
    meck:expect(exercise_search, search_by_name,[(<<"name">>)], {<<"name">>, <<"address">>}),
    ?assertEqual({<<"name">>, <<"address">>}, exercise_api:search_name(<<"name">>)).

search_address() ->
    meck:expect(exercise_search, search_by_http_address, [(<<"address">>)], {<<"name">>, <<"address">>}),
    ?assertEqual({<<"name">>, <<"address">>}, exercise_api:search_address(<<"address">>)).

search_text_in_page() ->
    meck:expect(exercise_search, search_page,
                [{<<"name">>, <<"address">>}, <<"text">>], [{<<"name">>, <<"address">>}, 1, <<"text">>]),
    ?assertEqual([{<<"name">>, <<"address">>}, 1, <<"text">>],
                 exercise_api:search_text({<<"name">>, <<"address">>}, <<"text">>)).

search_all_stored_pages() ->
    meck:expect(exercise_search, search_all_pages, [(<<"text">>)], [[{<<"name">>, <<"address">>}, 1, <<"text">>]]),
    ?assertEqual([[{<<"name">>, <<"address">>}, 1, <<"text">>]],exercise_api:search_text_all(<<"text">>)).

get_errors() ->
    meck:expect(exercise_scheduler, get_errors, [], [{"name","address","error"}]),
    ?assertEqual([{"name","address","error"}],exercise_api:get_errors()).