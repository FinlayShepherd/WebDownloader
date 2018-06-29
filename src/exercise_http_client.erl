%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017
%%% @doc
%%% Downloads a HTTP web page non recursively.
%%% @end
%%% Created : 17. Jul 2017 3:40 PM
%%%-------------------------------------------------------------------
-module(exercise_http_client).
-author("finlay.shepherd").

%% API
-export([download/1]).

%%--------------------------------------------------------------------
%% @doc
%% Makes a HTTP get request to HttpAddress
%% @param HttpAddress the address of the website
%%
%% @return ok and the data from the website or error and error message
%% @end
%%--------------------------------------------------------------------
-spec download(HttpAddress :: binary() | string()) ->
    {ok, Data :: binary()} | {error, Cause :: term()}.
download(HttpAddress) ->
    HttpString = exercise_util:to_string(HttpAddress),
    Response = httpc:request(get, {HttpString, []}, [{timeout, exercise_app:get_env(http_client_timeout, 5000)}],
                             [{body_format, binary}]),
    case Response of
        {ok, {{_, Status, ErrorDescription}, _, Data}} ->
            case Status of
                _ when Status >= 200 andalso Status =< 299 ->
                    {ok, Data};
                _ ->
                    {error, ErrorDescription}
            end;
        {error, Error} ->
            {error, Error}
    end.

