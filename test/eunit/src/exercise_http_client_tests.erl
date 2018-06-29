%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% exercise_http_client unit tests
%%% @end
%%% Created : 18. Jul 2017 3:11 PM
%%%-------------------------------------------------------------------
-module(exercise_http_client_tests).
-author("finlay.shepherd").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {
        foreach, fun() -> ok end,
        fun(_) -> meck:unload() end,
        [{"Download expecting string", fun successful_download/0},
         {"Download expecting binary", fun successful_download_binary/0},
         {"Download with error response", fun unsuccessful_download/0},
         {"Download with error response", fun unsuccessful_download2/0}]}.

successful_download() ->
    meck:expect(httpc, request,
                [get, {"http://www.erlang.org", []}, [{timeout, exercise_app:get_env(http_client_timeout,
                                                                                     5000)}], [{body_format, binary}]],
                {ok, {{"", 200, "ok"}, [], <<"data">>}}),

    ?assertEqual({ok, <<"data">>}, exercise_http_client:download("http://www.erlang.org")).

successful_download_binary() ->
    meck:expect(httpc, request,
                [get, {"http://www.erlang.org", []}, [{timeout, exercise_app:get_env(http_client_timeout,
                                                                                     5000)}], [{body_format, binary}]],
                {ok, {{"", 200, "ok"}, [], <<"data">>}}),
    ?assertEqual({ok, <<"data">>}, exercise_http_client:download(<<"http://www.erlang.org">>)).

unsuccessful_download() ->
    meck:expect(httpc, request,
                [get, {"http://www.erlang.org", []}, [{timeout, exercise_app:get_env(http_client_timeout,
                                                                                     5000)}], [{body_format, binary}]],
                {ok, {{"", 403, "Forbidden"}, [], "data"}}),
    ?assertEqual(exercise_http_client:download("http://www.erlang.org"), {error, "Forbidden"}).

unsuccessful_download2() ->
    meck:expect(httpc, request,
                [get, {"http://www.erlang.org", []}, [{timeout, exercise_app:get_env(http_client_timeout,
                                                                                     5000)}], [{body_format, binary}]],
                {error, failed_connect}),
    ?assertEqual(exercise_http_client:download("http://www.erlang.org"), {error, failed_connect}).
