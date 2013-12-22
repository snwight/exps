-module(index_od).

-export([consume/1, print/1]).

-define(PUNC, "\n\t\\\"\'.,!?@#$%+-=<>*^(){}[];:\/ ").

consume(FileName) when is_binary(FileName) ->
    case file:open(FileName, [raw, {read_ahead, 512}]) of
	{ok, FD} -> 
	    consume(FD);
	E = {error, _Reason} -> E
    end;
consume(FD) -> 
    consume(FD, orddict:new(), 0).

consume(FD, IDict, LineNum) ->
    case file:read_line(FD) of
	{ok, Line} ->
	    Tokens = string:tokens(Line, ?PUNC),
	    NewIDict = parse_line(Tokens, IDict, LineNum),
	    consume(FD, NewIDict, LineNum + 1);
	eof -> IDict;
	E = {error, _Reason} -> E
    end.

parse_line([], IDict, _LineNum) ->
    IDict;
parse_line([Tok|Line], IDict, LineNum) when length(Tok) < 5 ->
    parse_line(Line, IDict, LineNum);
parse_line([Tok|Line], IDict, LineNum) ->
    io:format("tok: ~p~n", [Tok]),
    NewIDict = 
	case orddict:is_key(Tok, IDict) of
	    true -> 
		orddict:append(Tok, LineNum, IDict);
	    false ->
		orddict:store(Tok, [LineNum], IDict)
	end,
    parse_line(Line, NewIDict, LineNum).

print(IDict) ->
    lists:foreach(
      fun({W, L}) -> 
	      LStr = string:right(
		       string:join([integer_to_list(J) || J <- L], ", "), 
		       80 - length(W)),
	      io:format("~s~s~n", [W, LStr])
      end, orddict:to_list(IDict)).

			  
