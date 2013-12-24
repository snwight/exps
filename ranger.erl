%%%-------------------------------------------------------------------
%%% @author Steve Wight <snwight@snwight.ubuntu.12.04>
%%% @copyright (C) 2013, Steve Wight
%%% @doc
%%%
%%% @end
%%% Created : 22 Dec 2013 by Steve Wight <snwight@snwight.ubuntu.12.04>
%%%-------------------------------------------------------------------
-module(ranger).

-behaviour(gen_server).

%% API
-export([lookup/1, add/1, delete/1, show/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ranges=[]}).

%%%===================================================================
%%% API
%%%===================================================================
lookup(Range) -> gen_server:call(ranger, {lookup, Range}).

add(Ranges) -> gen_server:cast(ranger, {add, Ranges}).

delete(Range) -> gen_server:cast(ranger, {delete, Range}).

show() -> gen_server:cast(ranger, show).
    
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) -> {ok, #state{}}.

handle_call({lookup, Ranges}, _From, State) ->
    Reply = lookup(Ranges, State#state.ranges),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add, Ranges}, State) ->
    {noreply, State#state{ranges = add(Ranges, State#state.ranges)}};
handle_cast({delete, Ranges}, State) ->
    {noreply, State#state{ranges = delete(Ranges, State#state.ranges)}};
handle_cast(show, State) ->
    io:format("Ranges: ~p~n", [State#state.ranges]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add([], Ranges) -> 
    Ranges;
add(Range={_, _}, Ranges) -> 
    add([Range], Ranges);
add([Range={Low, High}|T], Ranges) ->
    Lookup = lookup(Range, Ranges),
    NewRanges =
	case Lookup of
	    [Range] ->       %% same or enclosing range lready exists
		Ranges;          
	    [] ->                           %% insert brand new range
		lists:sort([Range|Ranges]);
	    Matches ->                      %% merge enclosing ranges 
		{L, _} = lists:nth(1, Matches),
		{_, H} = lists:last(Matches),
		GCRange = delete(Matches, Ranges),
		lists:sort([{min(Low, L), max(High, H)} | GCRange])
	end,
    add(T, NewRanges).

delete([], Ranges) -> 
    Ranges;
delete(Range={_, _}, Ranges) -> 
    delete([Range], Ranges);
delete([Range|T], Ranges) ->
    NewRange = lists:delete(Range, Ranges),
    delete(T, NewRange).

lookup({Low, High}, Ranges) ->
    Base = lists:dropwhile(fun({_, H}) -> H < Low end, Ranges),
    lists:takewhile(fun({L, _}) -> L =< High end, Base).
