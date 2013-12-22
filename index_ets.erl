-module(index_ets).
-export([consume/1, print/0]).

-define(PUNC, "\n\t\\\"\'.,!?@#$%+-=<>*^(){}[];:\/ ").
-define(INDEX, 'index').

consume(FileName) when is_binary(FileName) ->
    case file:open(FileName, [raw, {read_ahead, 512}]) of
	{ok, FD} -> 
	    consume(FD, ets:new(?INDEX, [bag, named_table, public]));
	E = {error, _Reason} -> E
    end.

consume(FD, Tbl) ->
    io:format("Tbl: ~p~n", [Tbl]),
    consume(FD, Tbl, 0).

consume(FD, Tbl, LineNum) ->
    case file:read_line(FD) of
	{ok, Line} ->
	    lists:foreach(
	      fun (Tok) when length(Tok) < 5 -> ok;
		  (Tok) -> 
		      io:format("tok: ~p~n", [Tok]),
		      true = ets:insert(Tbl, {Tok, LineNum})
	      end, string:tokens(Line, ?PUNC)),
	    consume(FD, Tbl, LineNum + 1);
	eof -> ok;
	E = {error, _Reason} -> E
    end.

sorted_keys() ->
    sorted_keys([], ets:first(?INDEX)).

sorted_keys(KeyList, '$end_of_table') ->
    lists:sort(KeyList);
sorted_keys(KeyList, W) ->
    sorted_keys([W|KeyList], ets:next(?INDEX, W)).

print() ->
    lists:foreach(
      fun(W) ->
	      L = lists:foldl(
		    fun({_W, LN}, Acc) -> [LN|Acc] end, [], ets:lookup(?INDEX, W)),
	      LStr = string:right(
		       string:join([integer_to_list(J) || J <- L], ", "), 
		       80 - length(W)),
	      io:format("~s~s~n", [W, LStr])
      end, sorted_keys()).


    
