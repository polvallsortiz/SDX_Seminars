-module(lock2).
-export([start/1]).

start(MyId) ->
    {master, 'master@Lenovito'} ! reg,
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId);
        stop ->
            ok
    end.

open(Nodes, MyId) ->
    receive
        %%WORKER WANTS TO ACCESS EXCLUSION ZONE
        {take, Master, Ref} ->
            Refs = requests(Nodes,MyId),
            wait(Nodes, Master, Refs, [], Ref, MyId);
        {request, From,  Ref, Id} ->
            From ! {ok, Ref},
            open(Nodes,MyId);
        stop ->
            ok
    end.

%%SEND ALL THE REQUESTS TO PEERS
requests(Nodes, MyId) ->
    lists:map(
        fun(P) ->
            R = make_ref(),
            P ! {request, self(), R, MyId},
            R
        end,
        Nodes).

%STATE WAITING ALL OKs
wait(Nodes, Master, [], Waiting, TakeRef, MyId) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId);

%%STATE WAITING
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId) ->
    receive
        {request, From, Ref, Id} ->
            if
                Id < MyId -> From ! {ok, Ref},
                    R = make_ref(),
                    From ! {request, self(), R, MyId},
                    wait(Nodes, Master, [R|Refs], Waiting, TakeRef, MyId);
                true -> wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId)
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),

            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId);
        release ->
            ok(Waiting),
            open(Nodes, MyId)
    end.

ok(Waiting) ->
    lists:map(
        fun({F,R}) ->
            F ! {ok, R}
        end,
        Waiting).

held(Nodes, Waiting, MyId) ->
    receive
        {request, From, Ref} ->
            held(Nodes, [{From, Ref}|Waiting],MyId);
        release ->
            ok(Waiting),
            open(Nodes, MyId)
    end.
