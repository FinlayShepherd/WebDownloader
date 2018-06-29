%%%-------------------------------------------------------------------
%%% @author finlay.shepherd
%%% @copyright (C) 2017
%%% @doc
%%% Gen_server used for key value storage
%%% @end
%%% Created : 14. Jul 2017 4:34 PM
%%%-------------------------------------------------------------------
-module(exercise_db).
-author("finlay.shepherd").

-behaviour(gen_server).

%% API
-export([start_link/0,
         put/2,
         get/1,
         update/2,
         delete/1,
         get_all_keys/0]).
%%export interface functions here

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {'database' = dict:new() :: dict:dict()}).
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
%% Stores a value in the database
%% @param Key key at which value will be stored
%% @param Value value to be stored in database
%%
%% @return ok or error
%% @end
%%--------------------------------------------------------------------
-spec put(Key :: term(), Value :: term()) ->
    ok | {error, Reason :: term()}.
put(Key, Value) ->
    gen_server:cast(?MODULE, {put, Key, Value}).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a value from the database
%% @param Key key at which value is stored
%%
%% @return ok and the value or error
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: term()) ->
    {ok, Value :: term()} | {error, not_found}.
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Updates a value within the database
%% @param Key key at which value is stored
%% @param Value the updated value
%%
%% @return ok and the previous value or error
%% @end
%%--------------------------------------------------------------------
-spec update(Key :: term(), Value :: term()) ->
    {ok, PreviousValue :: term()} | {error, not_found}.
update(Key, Value) ->
    gen_server:call(?MODULE, {update, Key, Value}).

%%--------------------------------------------------------------------
%% @doc
%% Deletes a value stored in the database
%% @param Key key at which value is stored
%%
%% @return ok and the deleted value or error
%% @end
%%--------------------------------------------------------------------
-spec delete(Key :: term()) ->
    {ok, PreviousValue :: term()} | {error, not_found}.
delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).


%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all the keys stored in the database, a key is comprised of {name, HttpAddress}
%%
%% @return List of keys stored in database
%% @end
%%--------------------------------------------------------------------
-spec get_all_keys() ->
    list([{Name :: term(), HttpAddress :: binary()}]).
get_all_keys() ->
    gen_server:call(?MODULE, get_keys).
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
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get, Key}, _From, #state{database = Db} = State) ->
    case dict:is_key(Key, Db) of
        true ->
            Value = dict:fetch(Key, Db),
            {reply, {ok, Value}, State};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call({update, Key, Value}, _From, #state{database = Db} = State) ->
    case dict:is_key(Key, Db) of
        true ->
            PrevValue = dict:fetch(Key, Db),
            NewDb = dict:store(Key, Value, Db),
            {reply, {ok, PrevValue}, State#state{database = NewDb}};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call({delete, Key}, _From, #state{database = Db} = State) ->
    case dict:is_key(Key, Db) of
        true ->
            PrevValue = dict:fetch(Key, Db),
            NewDb = dict:erase(Key, Db),
            {reply, {ok, PrevValue}, State#state{database = NewDb}};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call(get_keys, _From, #state{database = Db} = State) ->
    Keys = dict:fetch_keys(Db),
    {reply, Keys, State};
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
handle_cast({put, Key, Value}, #state{database = Db} = State) ->
    NewDb = dict:store(Key, Value, Db),
    {noreply, State#state{database = NewDb}};
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
