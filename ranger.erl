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

add(Range) -> gen_server:cast(ranger, {add, Range}).

delete(Range) -> gen_server:cast(ranger, {delete, Range}).

show() -> gen_server:cast(ranger, show).
    
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) -> {ok, #state{}}.

handle_call({lookup, Range}, _From, State) ->
    Reply = lookup(Range, State#state.ranges),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add, Range}, State) ->
    {noreply, State#state{ranges = add(Range, State#state.ranges)}};
handle_cast({delete, Range}, State) ->
    {noreply, State#state{ranges = delete(Range, State#state.ranges)}};
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
add(Range, Ranges) ->
    Lookup = lookup(Range, Ranges),
    io:format("lookup: ~p~n", [Lookup]),
    case Lookup of
	[{_,_}] -> ok;        %% same or enclosing range lready exists
	[] -> lists:sort([Range|Ranges]);    %% insert brand new range
	Matches = [{Lo,_}|T] ->     %% merge enclosing ranges into one 
	    {_,Hi} = lists:nthtail(1,T),
	    delete(Matches, Ranges),
	    lists:sort([{Lo, Hi}|Ranges])
    end.

delete([], _Ranges)     -> ok;
delete(R={_,_}, Ranges) -> lists:delete(R, Ranges);
delete([H|T], Ranges)   -> delete(T, lists:delete(H, Ranges)).

lookup({Low, High}, Ranges) ->
    Base = lists:dropwhile(fun({_L, H}) -> H < Low end, Ranges),
    lists:takewhile(fun({L, _H}) -> L =< High end, Base).
