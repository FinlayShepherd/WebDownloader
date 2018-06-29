%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, 
%%% @doc
%%% http server
%%% @end
%%% Created : 01. Aug 2017 12:48 PM
%%%-------------------------------------------------------------------
-module(exercise_http_server_api).
-author("finlay.shepherd").


%% API
-export([start_link/0, get/2, post/2, delete/2, put/2]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    mochiweb_http:start_link([{ip, exercise_app:get_env(http_server_listen_ip, any)},
                              {port, exercise_app:get_env(http_server_port, 6500)},
                              {loop, fun loop/1}]).

loop(Req) ->
    BPath = list_to_binary(Req:get(path)),
    Path = binary:split(BPath, <<"/">>, [global, trim_all]),
    Method = normalize_method(Req:get(method)),
    dispatch(Method, Path, Req).

%%--------------------------------------------------------------------
%% @doc
%% Deals with all requests sent to port 6500
%% Example urls:
%% /download/name?proto=?&addr=?&port=?&path=?
%% Downloads the web page specified.
%% /get?name=?&addr=?
%% Retrieves a record stored withing the database
%% /search_name/name
%% Searches the database for keys with a matching name
%% /search_address/address
%% Searches the database for keys with a matching address
%% /search_text?name=?&addr=?&search=?
%% Searches the specified file within the database for a string
%% /search_text_all/text
%% Searches all files stored in the database for a specified string
%% @param Req the request
%%
%% @return the http response
%% @end
%%--------------------------------------------------------------------
get([<<"download">>, Name], Req) ->
    Url = make_url(Req),
    exercise_api:download({Name, Url}),
    respond(Req, <<"ok">>, 200);
get([<<"get">>], Req) ->
    Key = make_key(Req),
    case exercise_api:get(Key) of
        {ok, Response} ->
            respond(Req, Response, 200);
        {error, _} ->
            respond(Req, "Not found", 400)
    end;
get([<<"search_name">>, SearchString], Req) ->
    Result = exercise_api:search_name(SearchString),
    respond(Req, format_result(Result, ""), 200);
get([<<"search_address">>, SearchString], Req) ->
    Result = exercise_api:search_address(SearchString),
    respond(Req, format_result(Result, ""), 200);
get([<<"search_text">>], Req) ->
    {Name, Addr, Search} = make_search(Req),
    Result = exercise_api:search_text({list_to_binary(Name), list_to_binary(Addr)}, list_to_binary(Search)),
    respond(Req, format_result(Result, ""), 200);
get([<<"search_text_all">>, SearchString], Req) ->
    Result = exercise_api:search_text_all(SearchString),
    respond(Req, format_result(Result, ""), 200);
get([<<"errors">>], Req) ->
    Errors = exercise_api:get_errors(),
    Response = io_lib:format("~p", [Errors]),
    respond(Req, list_to_binary(Response), 200);
get(_, Req) ->
    respond(Req, "not implemented", 501).

post(_, Req) ->
    respond(Req, "not implemented", 501).

delete(_, Req) ->
    respond(Req, "not implemented", 501).

put(_, Req) ->
    respond(Req, "not implemented", 501).

respond(Req, Content, Port) ->
    Req:respond({Port, [{<<"Content-Type">>, <<"text/html">>}], Content}).

dispatch(Method, Path, Req) ->
    erlang:apply(?MODULE, Method, [Path, Req]).

normalize_method('GET') ->
    get;
normalize_method('POST') ->
    post;
normalize_method('PUT') ->
    put;
normalize_method('DELETE') ->
    delete.

make_key(Req) ->
    [{"name", Name},{"addr", Address}] = Req:parse_qs(),
    {list_to_binary(Name), list_to_binary(Address)}.

make_search(Req) ->
    parse_search_qs(Req:parse_qs(), {"", "", ""}).

make_url(Req) ->
    {Proto, Addr, Port, Path} = parse_qs(Req:parse_qs()),
    Url = list_to_binary(Proto ++ Addr ++ Port ++ Path),
    Url.

parse_qs(List) ->
    parse_qs(List, {"http://", "localhost", "", ""}).

%%--------------------------------------------------------------------
%% @doc
%% Takes a query string and returns the values stored inside it.
%% @param List of values
%% @param Accumulator
%%
%% @return tuple containing the values or their defaults if value was not inputted
%% @end
%%--------------------------------------------------------------------
parse_search_qs([{"name", Value} | Rest], {_Name, Addr, Search}) ->
    parse_search_qs(Rest, {Value, Addr, Search});
parse_search_qs([{"addr", Value} | Rest], {Name, _Addr, Search}) ->
    parse_search_qs(Rest, {Name, Value, Search});
parse_search_qs([{"search", Value} | Rest], {Name, Addr, _Search}) ->
    parse_search_qs(Rest, {Name, Addr, Value});
parse_search_qs([], {Name, Addr, Search}) ->
    {Name, Addr, Search}.

parse_qs([{"proto", Value} | Rest], {_Proto, Addr, Port, Path}) ->
    parse_qs(Rest, {Value ++ "://", Addr, Port, Path});
parse_qs([{"addr", Value} | Rest], {Proto, _Addr, Port, Path}) ->
    parse_qs(Rest, {Proto, Value, Port, Path});
parse_qs([{"port", Value} | Rest], {Proto, Addr, _Port, Path}) ->
    parse_qs(Rest, {Proto, Addr, ":" ++ Value, Path});
parse_qs([{"path", Value} | Rest], {Proto, Addr, Port, _Path}) ->
    parse_qs(Rest, {Proto, Addr, Port, Value});
parse_qs([_ | Rest], {Proto, Addr, Port, Path}) ->
    parse_qs(Rest, {Proto, Addr, Port, Path});
parse_qs([], {Proto, Addr, Port, Path}) ->
    {Proto, Addr, Port, Path}.

%%--------------------------------------------------------------------
%% @doc
%% Takes results from API methods and returns appropriate response for http
%% @param List of data
%% @param Accumulator
%%
%% @return data in correct order with <p> tags.
%% @end
%%--------------------------------------------------------------------
format_result([], ResultString) ->
    list_to_binary(ResultString);
format_result([{Name, Address} | Rest], ResultString) ->
    NewResultString = "<p>" ++ binary_to_list(Name) ++ " , " ++ binary_to_list(Address) ++ "</p>" ++ ResultString,
    format_result(Rest, NewResultString);
format_result([{{Name, Address}, Line, Text} | Rest], ResultString) ->
    NewResultString = "<p>" ++ binary_to_list(Name) ++ ", " ++ binary_to_list(Address) ++ ", Line:" ++
                      integer_to_list(Line) ++ ", Text:" ++ binary_to_list(Text) ++ "</p>" ++ ResultString,
    format_result(Rest, NewResultString);
format_result([[] | Rest], ResultString) ->
    format_result(Rest, ResultString);
format_result([[{{Name, Address}, Line, Text} | Rest] | Rest2], ResultString) ->
    NewResultString = "<p>" ++ binary_to_list(Name) ++ ", " ++ binary_to_list(Address) ++ ", Line:" ++
                      integer_to_list(Line) ++ ", Text:" ++ binary_to_list(Text) ++ "</p>" ++ ResultString,
    format_result([Rest | Rest2], NewResultString).
