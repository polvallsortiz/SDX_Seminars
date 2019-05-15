-module(host).
-export([start/3, stop/1]).

start(Name, Domain, Parent) ->
    register(Name, spawn(fun()-> init(Name,Domain, Parent) end)).

stop(Name) ->
    Name ! stop,
    unregister(Name).

init(Name, Domain, Parent) ->
    io:format("Host: create domain ~w at ~w~n", [Domain, Parent]),
    Parent ! {register, Domain, {host, self()}},
    host(Name, Parent).

host(Name, Parent) ->
    receive
        {ping, From} ->
            io:format("Host: Ping from ~w~n", [From]),
            From ! pong,
            host(Name, Parent);
        stop ->
            io:format("Host: Closing down~n", []),
            Parent ! {deregister, Name},
            ok;
        Error ->
            io:format("Host: reception of strange message ~w~n", [Error]),
            host(Name, Parent)
    end.
