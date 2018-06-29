%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% A gen_server to schedule download jobs and then store the result in the database.
%%% @end
%%% Created : 19. Jul 2017 12:13 PM
%%%-------------------------------------------------------------------
-module(exercise_scheduler).
-author("finlay.shepherd").

-behaviour(gen_server).

%% API
-export([start_link/0,
         download/1,
         get_errors/0,
         flush_errors/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {'errors' = [] :: [{Name :: binary(), HttpAddress :: binary(), Error :: term()}]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Adds a download to the list of downloads
%% @param List a list of downloads containing their name and http address
%%
%% @return ok
%% @end
%%--------------------------------------------------------------------
-spec(download([{Name :: binary(), HttpAddress :: binary()}]) ->
    ok).
download(List) ->
    gen_server:cast(?MODULE, {download, List}).

%%--------------------------------------------------------------------
%% @doc
%% Sends each element of the list to execute_job
%% @param List a list of downloads containing their name and http address
%%
%% @return ok
%% @end
%%--------------------------------------------------------------------
-spec(execute_list([{Name :: binary(), HttpAddress :: binary()}]) ->
    ok).
execute_list(List) ->
    lists:foreach(fun execute_job/1, List).

%%--------------------------------------------------------------------
%% @doc
%% Calls download in http client and then stores the result and the name in the database,
%% checks the response of the download client and acts accordingly.
%% @param {Name, HttpAddress} A tuple containing the data needed to complete the request
%%
%% @return ok
%% @end
%%--------------------------------------------------------------------
-spec(execute_job({Name :: binary(), HttpAddress :: binary()}) ->
    ok).
execute_job({Name, HttpAddress}) ->
    gen_server:cast(?MODULE, {execute_job, {Name, HttpAddress}}).

%%--------------------------------------------------------------------
%% @doc
%% Gets a list of all errors that have occured
%%
%% @return List of tuples containing the Name, HttpAddress and error types of unsuccessfull requests
%% @end
%%--------------------------------------------------------------------
-spec(get_errors() ->
    [{Name :: binary(), HttpAddress :: binary(), Error :: term()}]).
get_errors() ->
    gen_server:call(?MODULE, {get_errors}).

%%--------------------------------------------------------------------
%% @doc
%% Removes all errors from the state
%%
%% @return ok
%% @end
%%--------------------------------------------------------------------
-spec(flush_errors() ->
    ok).
flush_errors() ->
    gen_server:cast(?MODULE, {flush_errors}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get_errors}, _From, #state{errors = Errors} = State) ->
    {reply, Errors, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({download, NewJobs}, State) ->
    execute_list(NewJobs),
    {noreply, State};
handle_cast({execute_job, {Name, HttpAddress}}, #state{errors = ErrorList} = State) ->
    case exercise_http_client:download(HttpAddress) of
        {ok, Data} ->
            exercise_db:put({Name, HttpAddress}, Data),
            {noreply, State};
        {error, Description} ->
            NewErrors = [{Name, HttpAddress, Description} | ErrorList],
            {noreply, State#state{errors = NewErrors}}
    end;
handle_cast({flush_errors}, #state{}) ->
    {noreply, #state{errors = []}};
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
