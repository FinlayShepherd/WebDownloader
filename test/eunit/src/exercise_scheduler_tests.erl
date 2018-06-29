%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <Alert Logic>
%%% @doc
%%% Unit testing for the exercise_scheduler module
%%% @end
%%% Created : 19. Jul 2017 4:12 PM
%%%-------------------------------------------------------------------
-module(exercise_scheduler_tests).
-author("finlay.shepherd").

-include_lib("eunit/include/eunit.hrl").

setup() ->
    exercise_scheduler:start_link().

all_test_() ->
    {setup, fun setup/0,
        fun(_) ->
            meck:unload()
        end, [{"Successful download test", fun download_success/0},
              {"Unsuccessful download test", fun download_failure/0},
              {"Flush errors test", fun flush_errors/0}]}.

download_success() ->
    meck:expect(exercise_http_client, download, fun(<<"http://www.erlang.org">>) -> {ok, <<"data">>} end),
    meck:expect(exercise_db, put, fun({<<"name">>,<<"http://www.erlang.org">>}, <<"data">>) -> ok end),
    ?assertEqual(exercise_scheduler:download([{<<"name">>, <<"http://www.erlang.org">>}]), ok),
    timer:sleep(1000),
    ?assert(meck:called(exercise_http_client, download, 1)),
    ?assert(meck:called(exercise_db, put, 2)).

download_failure() ->
    meck:expect(exercise_http_client, download, fun(<<"http://www.erlang.org">>) -> {error, "404"} end),
    exercise_scheduler:download([{<<"erlang">>, <<"http://www.erlang.org">>}]),
    timer:sleep(1000),
    ?assertEqual([{<<"erlang">>, <<"http://www.erlang.org">>, "404"}], exercise_scheduler:get_errors()).

flush_errors() ->
    meck:expect(exercise_http_client, download, fun(<<"http://www.erlang.org">>) -> {error, "404"} end),
    exercise_scheduler:download([{<<"erlang">>, <<"http://www.erlang.org">>}]),
    timer:sleep(1000),
    exercise_scheduler:flush_errors(),
    ?assertEqual([], exercise_scheduler:get_errors()).