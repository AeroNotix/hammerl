-module(stats).

-behaviour(gen_server).

%% API
-export([start_link/0, inc/1, total/1, all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {totals = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

inc(What) ->
    gen_server:cast(?SERVER, {inc, What}).

total(What) ->
    gen_server:call(?SERVER, {total, What}).

all() ->
    gen_server:call(?SERVER, all).

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
init([]) ->
    {ok, #state{}}.

%% @doc
%% handle_call({total, What}), _From, State) will return the total
%% amount for the What you pass in.
%% @spec handle_call({total, What}, _Form, State) -> integer() | {error, Reason}
handle_call({total, What}, _From, State) ->
    Total = current(What, State),
    {reply, Total, State};

%% @doc
%% This will return a list with a {Key, Value} for each of the entries
%% in the map
%% @spec handle_call(all, _From, State) -> list()
handle_call(all, _From, State) ->
    {reply, dict:to_list(State#state.totals), State}.


%% @doc
%% This will increment the counter for one element in the table.
handle_cast({inc, What}, State) ->
    State2 = #state{totals = dict:store(What, current(What, State)+1, State#state.totals)},
    {noreply, State2}.

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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

current(What, State) ->
    case dict:is_key(What, State#state.totals) of
        true ->
            dict:fetch(What, State#state.totals);
        false ->
            0
    end.
