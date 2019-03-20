-module(lock3).
-export([start/1]).

start(MyId) ->
  {master, 'muty@ASUSPol.upc.edu'} ! reg,
  spawn(fun() -> init(MyId) end).

init(_) ->
  receive
    {peers, Nodes} ->
      open(Nodes, 0);
    stop ->
      ok
  end.

open(Nodes, Clock) ->
  receive
    {take, Master, Ref} ->
      +clock,
      CurrentTime = clock,
      Refs = requests(Nodes, Clock, CurrentTime),
      wait(Nodes, Master, Refs, [], Ref, Clock, CurrentTime);
    {request, From,  Ref, RefClock, RefTime} ->
      clock = max(Clock, RefClock),
      From ! {ok, Ref},
      open(Nodes);
    stop ->
      ok
  end.

requests(Nodes, Clock) ->
  lists:map(
    fun(P) ->
      R = make_ref(),
      P ! {request, self(), R, Clock, },
      R
    end,
    Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, Clock, CurrentTime) ->
  Master ! {taken, TakeRef},
  held(Nodes, Waiting, Clock);

wait(Nodes, Master, Refs, Waiting, TakeRef, Clock, CurrentTime) ->
  receive
    {request, From, Ref, RefClock, RefTime} ->
      if
        RefTime < CurrentTime -> From ! {ok, Ref},
          wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, Clock, CurrentTime);
        true -> wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, Clock, CurrentTime)
      end;
    {ok, Ref} ->
      NewRefs = lists:delete(Ref, Refs),
      wait(Nodes, Master, NewRefs, Waiting, TakeRef, Clock, CurrentTime);
    release ->
      ok(Waiting),
      open(Nodes, Clock)
  end.

ok(Waiting) ->
  lists:map(
    fun({F,R}) ->
      F ! {ok, R}
    end,
    Waiting).

held(Nodes, Waiting, Clock) ->
  receive
    {request, From, Ref, RefClock, RefTime} ->
      held(Nodes, [{From, Ref}|Waiting], Clock);
    release ->
      ok(Waiting),
      open(Nodes, Clock)
  end.
