-module(cache).
-export([lookup/2, add/4,remove/2]).

lookup(Req, Entries) ->	
	Found = lists:keyfind(Req, 1, Entries),
	case Found of
		false ->
			unknown;
		{_,Return,X} -> 
			Now = erlang:convert_time_unit(erlang:monotonic_time(), native, second),
			if X < Now ->
				invalid;
			true -> Return
			end
	end.

add(Name, Expire, Entry, Cache) ->
	lists:keystore(Name, 1, Cache, {Name,Entry,Expire}).
	
remove(Name, Cache) ->
	lists:keydelete(Name, 1, Cache).

		
	

