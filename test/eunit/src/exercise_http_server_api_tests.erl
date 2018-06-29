%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <Alert Logic>
%%% @doc
%%% Unit tests for http server api
%%% @end
%%% Created : 04. Aug 2017 10:55 AM
%%%-------------------------------------------------------------------
-module(exercise_http_server_api_tests).
-author("finlay.shepherd").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach,
        fun() ->
            meck:new([exercise_api]),
            {ok, Pid} = exercise_http_server_api:start_link(),
            Pid
        end,
        fun(Pid) ->
            meck:unload(),
            process_flag(trap_exit, true),
            exit(Pid, kill),
            receive
                {'EXIT', Pid, _} -> ok
            after
                1000 -> ok
            end,
            process_flag(trap_exit, false)
        end, [{"test download path", fun download/0},
              {"test valid get path", fun get/0},
              {"test invalid get path", fun bad_get/0},
              {"test name search", fun search_name/0},
              {"test address search", fun search_address/0},
              {"test text search", fun search_text/0},
              {"test search all text", fun search_all_text/0},
              {"test with invalid protocol1", fun post/0},
              {"test with invalid protocol2", fun put/0},
              {"test with invalid protocol3", fun delete/0}]}.

download() ->
    meck:expect(exercise_api, download, [{<<"name">>, <<"http://www.testwebsite.com">>}], ok),
    {_, {_, _, Response}} =
        httpc:request(get, {"http://localhost:6500/download/name?addr=www.testwebsite.com", []}, [], []),
    ?assertEqual("ok", Response).

get() ->
    meck:expect(exercise_api, get, [{<<"name">>, <<"http://www.testwebsite.com">>}], {ok, <<"Website content">>}),
    {_, {_, _, Response}} =
        httpc:request(get, {"http://localhost:6500/get?name=name&addr=http://www.testwebsite.com", []}, [], []),
    ?assertEqual("Website content", Response).

bad_get() ->
    meck:expect(exercise_api, get, [{<<"name">>, <<"http://www.testwebsite.com">>}], {error, not_found}),
    {_, {_, _, Response}} =
        httpc:request(get, {"http://localhost:6500/get?name=name&addr=http://www.testwebsite.com", []}, [], []),
    ?assertEqual("Not found", Response).

search_name() ->
    meck:expect(exercise_api, search_name, [<<"test">>], [{<<"test">>, <<"httpaddress">>}]),
    {_, {_, _, Response}} = httpc:request(get, {"http://localhost:6500/search_name/test", []}, [], []),
    ?assertEqual("<p>test , httpaddress</p>", Response).

search_address() ->
    meck:expect(exercise_api, search_address, [<<"httpaddress">>], [{<<"test">>, <<"httpaddress">>}]),
    {_, {_, _, Response}} = httpc:request(get, {"http://localhost:6500/search_address/httpaddress", []}, [], []),
    ?assertEqual("<p>test , httpaddress</p>", Response).

search_text() ->
    meck:expect(exercise_api, search_text, [{<<"name">>, <<"httpaddress">>}, <<"text">>],
                [{{<<"name">>, <<"httpaddress">>}, 1, <<"text">>}]),
    {_, {_, _, Response}} =
        httpc:request(get, {"http://localhost:6500/search_text?name=name&addr=httpaddress&search=text", []}, [], []),
    ?assertEqual("<p>name, httpaddress, Line:1, Text:text</p>", Response).

search_all_text() ->
    meck:expect(exercise_api, search_text_all, [<<"text">>],
                [[{{<<"name">>, <<"httpaddress">>}, 1, <<"text">>}],
                 [{{<<"name2">>, <<"httpaddress2">>}, 2, <<"text">>}]]),
    {_, {_, _, Response}} = httpc:request(get, {"http://localhost:6500/search_text_all/text", []}, [], []),
    ?assertEqual("<p>name2, httpaddress2, Line:2, Text:text</p><p>name, httpaddress, Line:1, Text:text</p>", Response).

post() ->
    {_, {_, _, Response}} =
        httpc:request(post, {"http://localhost:6500/download/name?addr=www.testwebsite.com", [], [], []}, [], []),
    ?assertEqual("not implemented", Response).

put() ->
    {_, {_, _, Response}} =
        httpc:request(put, {"http://localhost:6500/download/name?addr=www.testwebsite.com", [], [], []}, [], []),
    ?assertEqual("not implemented", Response).
delete() ->
    {_, {_, _, Response}} =
        httpc:request(delete, {"http://localhost:6500/download/name?addr=www.testwebsite.com", []}, [], []),
    ?assertEqual("not implemented", Response).
