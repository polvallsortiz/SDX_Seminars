-module(cache).
-export([lookup/2, add/4,remove/2, purge/1]).

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

purge([],NewCache) ->
	NewCache;

purge([H|T], NewCache) ->
	case H of
		{Name, _, Expire} ->
			Now = erlang:convert_time_unit(erlang:monotonic_time(), native, second),
			if Expire < Now ->
				NewIntCache = remove(Name, NewCache),
				purge(T,NewIntCache);
			true ->
				purge(T,NewCache)
			end
	end.



purge(Cache) ->
	io:format("Cache: purge~n"),
	NewCache = purge(Cache,Cache),
	io:format("Cache: returning ~w~n", [NewCache]),
	NewCache.

	
	

		
	

