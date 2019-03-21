-module(causal).
-export([start/3]).

start(Id, Master, Jitter) ->
    spawn(fun() -> init(Id, Master, Jitter) end).

init(Id, Master, Jitter) ->
    receive
        {peers, Nodes} ->
			VC = newVC(length(Nodes), []),
            server(Id, Master, lists:delete(self(), Nodes), Jitter,VC, [])
    end.

server(Id, Master, Nodes, Jitter, VC, Queue) ->
    receive
        {send, Msg, VC} ->
			setelement(Id-1, VC, element(Id-1, VC)+1),
            multicast(Msg, Nodes, Jitter, Id, VC),
            Master ! {deliver, Msg},
            server(Id, Master, Nodes, Jitter, VC, Queue);
            
		{multicast, Msg, FromId, MsgVC} ->
			case checkMsg(FromId, MsgVC, VC, size(VC)) of
			ready ->
			%% TODO: ADD SOME CODE
				NewVC =  maxVC(VC,MsgVC,length(VC)),%% TODO: COMPLETE
				{NewerVC, NewQueue} = deliverReadyMsgs(Master, NewVC, Queue, Queue),
				server(Id, Master, Nodes, Jitter, NewerVC, NewQueue);
			wait ->
				server(Id, Master, Nodes, Jitter, VC, [{FromId, MsgVC, Msg}|Queue])
			end;

        stop ->
            ok
    end.

multicast(Msg, Nodes, 0, Id, VC) ->
    lists:foreach(fun(Node) -> 
                      Node ! {multicast, Msg, Id, VC} 
                  end, 
                  Nodes);
multicast(Msg, Nodes, Jitter, Id, VC) ->
    lists:foreach(fun(Node) -> 
                      T = rand:uniform(Jitter),
                      timer:send_after(T, Node, {multicast, Msg, Id, VC})
                  end, 
                  Nodes).

newVC(0, List) ->
	list_to_tuple(List);
	
newVC(N, List) ->
	newVC(N-1, [0|List]).
	
%% Check if a message can be delivered to the master
checkMsg(_, _, _, 0) -> ready;

checkMsg(FromId, MsgVC, VC, FromId) ->
	if (element(FromId-1,VC)+1 == element(FromId-1,MsgVC)) -> %% TODO: COMPLETE	
			checkMsg(FromId, MsgVC, VC, FromId-1);
		true -> wait
	end;
		
checkMsg(FromId, MsgVC, VC, N) ->
	if (element(FromId-1,MsgVC) =< element(FromId-1,VC)) -> %% TODO: COMPLETE
			checkMsg(FromId, MsgVC, VC, N-1);
		true -> wait
	end.
	
%% Deliver to the master all the ready messages in the hold-back queue
deliverReadyMsgs(_, VC, [], Queue) ->
	{VC, Queue};
	
deliverReadyMsgs(Master, VC, [{FromId, MsgVC, Msg}|Rest], Queue) ->
	case checkMsg(FromId, MsgVC, VC, size(VC)) of
		ready ->
			%% TODO: ADD SOME CODE
			NewVC = maxVC(VC,MsgVC,length(VC)),
			NewQueue = lists:delete({FromId, MsgVC, Msg}, Queue),
			deliverReadyMsgs(Master, NewVC, NewQueue, NewQueue);
		wait ->
			deliverReadyMsgs(Master, VC, Rest, Queue)
	end.

maxVC(VC1,VC2,Pos) ->
	if
		Pos == 0 -> VC1;
		Pos > 0 -> 
			Aux = element(Pos-1,VC1),
			Aux2 = element(Pos-1,VC2),
			Max = max(Aux,Aux2),			
			setelement(Pos-1, VC1, Max),
			maxVC(VC1,VC2,Pos-1)
	end.
