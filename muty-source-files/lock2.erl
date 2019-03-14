-module(lock1).
-export([start/1]).

start(MyId) ->
    {master, 'master@c6s303pc21.fib.upc.es'} ! reg,
    spawn(fun() -> init(MyId) end).

init(Id) ->
    receive
        {peers, Nodes} ->
            open(Nodes, Id);
        stop ->
            ok
    end.

open(Nodes, Id) ->
    receive
        {take, Master, Ref} ->
            Refs = requests(Nodes),
            wait(Nodes, Master, Refs, [], Ref, Id);
        {request, From,  Ref} ->
            From ! {ok, Ref},
            open(Nodes);
        stop ->
            ok
    end.

requests(Nodes) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R}, 
        R 
      end, 
      Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, Id) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, Id);

wait(Nodes, Master, Refs, Waiting, TakeRef, Id) ->
    receive
        {request, From, Ref, FromId} ->
            if
                FromId < Id ->
                    wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, Id),
                    R = make_ref(),
                    {From, Ref} ! {ok, R};
                Id > 0 -> 
                    wait(Node,Master,Refs,Waiting,TakeRef, Id)     
            end.
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, Id);
        release ->
            ok(Waiting),            
            open(Nodes)
    end.

ok(Waiting) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

held(Nodes, Waiting, Id) ->
    receive
        {request, From, Ref} ->
            held(Nodes, [{From, Ref}|Waiting], Id);
        release ->
            ok(Waiting),
            open(Nodes, Id)
    end.
