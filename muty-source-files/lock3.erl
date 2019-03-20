-module(lock3).
-export([start/1]).

start(MyId) ->
  {master, 'master@Lenovito'} ! reg,
  spawn(fun() -> init(MyId) end).

init(MyId) ->
  receive
    {peers, Nodes} ->
      open(Nodes, MyId, 0);
    stop ->
      ok
  end.

open(Nodes, MyId, Clock) ->
  receive
    {take, Master, Ref} ->
      CurrentTime = Clock + 1,
      Refs = requests(Nodes, MyId, CurrentTime),
      wait(Nodes, Master, Refs, [], Ref, MyId, CurrentTime, CurrentTime);
    {request, From,  Ref, RefClock, Time} ->
      MaxClock = max(Time,Clock),
      From ! {ok, Ref},
      open(Nodes, MyId, MaxClock);
    stop ->
      ok
  end.

requests(Nodes, MyId, Clock) ->
  lists:map(
    fun(P) ->
      R = make_ref(),
      P ! {request, self(), R, MyId, Clock},
      R
    end,
    Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, MyId, Clock, CurrentTime) ->
  Master ! {taken, TakeRef},
  held(Nodes, Waiting, MyId, CurrentTime);

wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, Clock, CurrentTime) ->
  receive
    {request, From, Ref, FromId, Time} ->
      MaxClock = max(Time,CurrentTime),
      if        
       Time < Clock -> 
			From ! {ok, Ref},
			wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId, Clock, MaxClock);
       Time == Clock, FromId < MyId -> 
			From ! {ok, Ref},
			wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId, Clock, MaxClock);
      true -> wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId, Clock, MaxClock)
     end;
    {ok, Ref} ->
      NewRefs = lists:delete(Ref, Refs),
      wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, Clock, CurrentTime);
    release ->
      ok(Waiting),
      open(Nodes, MyId, CurrentTime)
  end.

ok(Waiting) ->
  lists:map(
    fun({F,R}) ->
      F ! {ok, R}
    end,
    Waiting).

held(Nodes, Waiting, MyId, Clock) ->
  receive
    {request, From, Ref, RefClock, Time} ->
      MaxClock = max(Time,Clock), 
      held(Nodes, [{From, Ref}|Waiting], MyId, MaxClock);
    release ->
      ok(Waiting),
      open(Nodes, MyId, Clock)
  end.
