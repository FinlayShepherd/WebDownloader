%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <Alert Logic>
%%% @doc
%%% exercise_db unit tests
%%% @end
%%% Created : 17. Jul 2017 11:15 AM
%%%-------------------------------------------------------------------
-module(exercise_db_tests).
-author("finlay.shepherd").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {
        foreach,
        fun() ->
            {ok, Pid} = exercise_db:start_link(),
            Pid
        end,
        fun(Pid) ->
            process_flag(trap_exit, true),
            exit(Pid, kill),
            receive
                {'EXIT', Pid, _} -> ok
            after
                1000 -> ok
            end,
            process_flag(trap_exit, false)
        end,
        [
            {"Adding and retrieving a value from db", fun put_and_get/0},
            {"Updating a value in db", fun update_ok/0},
            {"Delete a value in db", fun delete_ok/0},
            {"Update value that doesnt exist", fun update_error/0},
            {"Delete value that doesnt exist", fun delete_error/0},
            {"Retrieve list of keys from db", fun get_all_keys_ok/0}
        ]
    }.

put_and_get() ->
    exercise_db:put({"erlang", <<"http://www.erlang.org">>}, <<"Page Content">>),
    ?assertEqual({ok, <<"Page Content">>}, exercise_db:get({"erlang", <<"http://www.erlang.org">>})).

update_ok() ->
    exercise_db:put(1, finlay),
    exercise_db:update(1, james),
    ?assertEqual({ok, james}, exercise_db:get(1)).

delete_ok() ->
    exercise_db:put(1, finlay),
    exercise_db:delete(1),
    ?assertEqual({error, not_found}, exercise_db:get(1)).

update_error() ->
    ?assertEqual(exercise_db:update(1, james), {error, not_found}).

delete_error() ->
    ?assertEqual(exercise_db:delete(1), {error, not_found}).

get_all_keys_ok() ->
    exercise_db:put({"erlang", <<"http://www.erlang.org">>}, <<"Page Content">>),
    exercise_db:put({"google", <<"http://www.google.com">>}, <<"More Page Content">>),
    ?assertEqual([{"erlang", <<"http://www.erlang.org">>}, {"google", <<"http://www.google.com">>}],
                 exercise_db:get_all_keys()).