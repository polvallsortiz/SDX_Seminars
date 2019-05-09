-module(server).
-export([start/0, start/2, stop/0]).

start() ->
    register(server, spawn(fun()-> init() end)).

start(Domain, Parent) ->
    register(server, spawn(fun()-> init(Domain, Parent) end)).

stop() ->
    server ! stop,
    unregister(server).

init() ->
    io:format("Server: create root domain~n"),
    server(null, root, [], 0).

init(Domain, Parent) ->
    io:format("Server: create domain ~w at ~w~n", [Domain, Parent]),
    Parent ! {register, Domain, {domain, self()}},
    server(Parent, Domain, [], 0).

server(Parent, Domain, Entries, TTL) ->
    receive
        {request, From, Req}->
            io:format("Server: received request to solve [~w]~n", [Req]),
            Reply = entry:lookup(Req, Entries),
            From ! {reply, Reply, TTL},
            server(Parent, Domain, Entries, TTL);
        {register, Name, Entry} ->
            NewEntries = entry:add(Name, Entry, Entries),
            server(Parent, Domain, NewEntries, TTL);
        {deregister, Name} ->
            NewEntries = entry:remove(Name, Entries),
            server(Parent, Domain, NewEntries, TTL);
        {ttl, Sec} ->
            server(Parent, Domain, Entries, Sec);
        status ->
            io:format("Server: List of DNS entries: ~w~n", [Entries]),
            server(Parent, Domain, Entries, TTL);
        stop ->
            io:format("Server: closing down~n", []),
            if 
				Domain /= root ->
					Parent ! {deregister, Domain};
				true -> ok
			end,
            ok;
        Error ->
            io:format("Server: reception of strange message ~w~n", [Error]),
            server(Parent, Domain, Entries, TTL)
    end.
