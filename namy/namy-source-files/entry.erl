-module(entry).
-export([lookup/2, add/3,remove/2]).

lookup(Req, Entries) ->
	Found = lists:keyfind(Req, 1, Entries),
	case Found of
		false ->
			unknown;
		{_,Return} -> Return
	end.

add(Name, Entry, Entries) ->
	lists:keystore(Name, 1, Entries, {Name,Entry}).
	
remove(Name, Entries) ->
	lists:keydelete(Name, 1, Entries).

		
	

