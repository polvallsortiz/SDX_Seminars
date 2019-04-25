-module(gms3).
-export([start/1, start/2]).

start(Name) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
    leader(Name, Master, [], 1).

start(Name, Grp) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Grp, Self) end).    

init(Name, Grp, Master) ->
    Self = self(), 
    Grp ! {join, Self},
    receive
        {view, Leader, Slaves} ->
            Master ! joined,
            Ref = erlang:monitor(process,Leader),
            slave(Name, Master, Leader, Slaves, Ref, 1, {})
    after 1000 ->
        Master ! {error, "no reply from leader"}
    end.

leader(Name, Master, Slaves, N) ->    
    receive
        {mcast, Msg} ->
            bcast(Name, {msg,Msg, N}, Slaves),  %% TODO: COMPLETE
            %% TODO: ADD SOME CODE
            Master ! {deliver,Msg},
            leader(Name, Master, Slaves, N);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast(Name,  {view, self(), NewSlaves, N}, NewSlaves),  %% TODO: COMPLETE
            
            leader(Name, Master, NewSlaves,N);  %% TODO: COMPLETE
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.
    

bcast(Name, Msg, Nodes) ->
    lists:foreach(fun(Node) ->
                    Node ! Msg,
                    crash(Name, Msg)
                end,
                Nodes).


crash(Name, Msg) ->
    case rand:uniform(100) of
        10 ->
            io:format("leader ~s CRASHED: msg ~w~n", [Name, Msg]),
            exit(no_luck);
        _ ->
            ok
    end.

slave(Name, Master, Leader, Slaves, Ref, N, Last) ->    
    receive
        {mcast, Msg} ->
            %% TODO: ADD SOME CODE
            Leader ! {mcast, Msg},
            slave(Name, Master, Leader, Slaves, Ref, N, Last);
        {join, Peer} ->
            %% TODO: ADD SOME CODE
            Leader ! {join,Peer},
            slave(Name, Master, Leader, Slaves, Ref, N, Last);
        {msg, Msg, N} -> 
            Master ! {deliver, Msg},
            slave(Name, Master, Leader, Slaves, Ref, N+1, {msg, Msg, N});
        {msg, Msg, I} when I < N -> 
            slave(Name, Master, Leader, Slaves, Ref, N, Last);        
        {view, Leader, NewSlaves, N} ->
            erlang:demonitor(Ref, [flush]),
            NewRef = erlang:monitor(process, Leader),
            slave(Name, Master, Leader, NewSlaves, NewRef, N+1, {view, Leader, NewSlaves, N});  %% TODO: COMPLETE
        {view, Leader, NewSlaves, I} when I < N ->
            slave(Name, Master, Leader, Slaves, Ref, N, Last);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Name, Master, Slaves, N, Last);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.

election(Name, Master, Slaves, N, Last) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Name, Last, Rest),
            bcast(Name, {view, Self, Rest}, Rest),
            leader(Name, Master, Rest, N);  %% TODO: COMPLETE
        [NewLeader|Rest] ->
            Ref = erlang:monitor(process, NewLeader),
            slave(Name, Master, NewLeader, Rest, Ref, N, Last)  %% TODO: COMPLETE
    end.
