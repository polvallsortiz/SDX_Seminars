-module(gms2).
-export([start/1, start/2]).

start(Name) ->
    Self = self(),
    spawn_link(fun()-> init(Name, Self) end).

init(Name, Master) ->
    leader(Name, Master, []).

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
            leader(Name, Master, Slaves, N+1);
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast(Name,  {view, self(), NewSlaves, N}, NewSlaves),  %% TODO: COMPLETE
            
            leader(Name, Master, NewSlaves);  %% TODO: COMPLETE
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error])
    end.
    
bcast(Name, Msg, Nodes) ->
	lists:foreach(fun(Node) ->
			Node ! Msg,
			 
		end,
		Nodes).
		
crash(Name, Msg) ->
	case rand:uniform(?arghh) of
		?arghh ->
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
            slave(Name, Master, Leader, Slaves, Ref);
        {join, Peer} ->
            %% TODO: ADD SOME CODE
            Leader ! {join,Peer},
            slave(Name, Master, Leader, Slaves, Ref, N, );
        {msg, Msg, N} ->
            %% TODO: ADD SOME CODE
            Master ! {deliver, Msg},
            slave(Name, Master, Leader, Slaves, Ref);
        {view, Leader, NewSlaves, N} ->
            erlang:demonitor(Ref, [flush]),
			NewRef = erlang:monitor(process, Leader),
			slave(Name, Master, Leader, NewSlaves, NewRef, N L);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Name, Master, Slaves);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error])
    end.
election(Name, Master, Slaves, N, Last) ->
	Self = self(),
	case Slaves of
	[Self|Rest] ->
		bcast(Name, {view, Self, Rest}, Rest), 
		leader(Name, Master, Rest); %% TODO: COMPLETE
	[NewLeader|Rest] ->
		%% TODO: ADD SOME CODE
		Ref = erlang:monitor(process, NewLeader),
		slave(Name, Master, NewLeader, Rest, Ref) %% TODO: COMPLETE
end.
