-module(node3).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(MyKey) ->
  start(MyKey, nil).

start(MyKey, PeerPid) ->
  timer:start(),
  spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
  Predecessor = nil,
  {ok, Successor} = connect(MyKey, PeerPid),
  Store = storage:create(),
  schedule_stabilize(),
  node(MyKey, Predecessor, Successor, nil, Store).

connect(MyKey, nil) ->
  {ok, {MyKey , nil, self()}};    %% TODO: ADD SOME CODE
connect(_, PeerPid) ->
  Sref = monit(PeerPid),
  Qref = make_ref(),
  PeerPid ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey , Sref, PeerPid}}    %% TODO: ADD SOME CODE
  after ?Timeout ->
    io:format("Timeout: no response from ~w~n", [PeerPid])
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Next, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, MyKey},
      node(MyKey, Predecessor, Successor, Next, Store);
    {notify, NewPeer} ->
      {NewPredecessor, NewStore} = notify(NewPeer, MyKey, Predecessor, Store),
      node(MyKey, NewPredecessor, Successor, Next, NewStore);
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(MyKey, Predecessor, Successor, Next, Store);
    {status, Pred, Nx} ->
      {NewSuccessor, NewNext} = stabilize(Pred, Nx, MyKey, Successor),
      node(MyKey, Predecessor, NewSuccessor, NewNext, Store);
    stabilize ->
      stabilize(Successor),
      node(MyKey, Predecessor, Successor, Next, Store);
    stop ->
      ok;
    probe ->
      create_probe(MyKey, Successor),
      node(MyKey, Predecessor, Successor, Next, Store);
    {probe, MyKey, Nodes, T} ->
      remove_probe(MyKey, Nodes, T, Store),
      node(MyKey, Predecessor, Successor, Next, Store);
    {probe, RefKey, Nodes, T} ->
      forward_probe(RefKey, [MyKey|Nodes], T, Successor),
      node(MyKey, Predecessor, Successor, Next, Store);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
      node(MyKey, Predecessor, Successor, Next, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
      node(MyKey, Predecessor, Successor, Next, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(MyKey, Predecessor, Successor, Next, Merged);
    {'DOWN', Ref, process, _, _} ->
      {NewPred, NewSucc, NewNext} = down(Ref, Predecessor, Successor, Next),
      node(MyKey, NewPred, NewSucc, NewNext, Store)

end.

stabilize(Pred, Nx, MyKey, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
    nil ->
      %% TODO: ADD SOME CODE
      Spid ! {notify, {MyKey, self()} },
      {Successor, Nx};
    {MyKey, _, _} ->
      {Successor, Nx};
    {Skey, _, _} ->
      Spid ! {notify, {MyKey, self()} },
      %% TODO: ADD SOME CODE
      {Successor, Nx};
    {Xkey, _, Xpid} ->
      case key:between(Xkey, MyKey, Skey) of
        true ->
          self() ! stabilize,
          demonit(Sref),
          Xref = monit(Xpid),
          {{Xkey, Xref, Xpid}, {Skey, Spid}};
        %% TODO: ADD SOME CODE
        %% TODO: ADD SOME CODE
        false ->
          %% TODO: ADD SOME CODE
          Spid ! {notify, {MyKey, self()} },
          {Successor, Nx}
      end
  end.

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor, {Skey, _, Spid}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, Pref, Ppid} ->
      Peer ! {status, {Pkey, Pref, Ppid}, {Skey, Spid}}
  end.

%%notify({Nkey, Npid}, MyKey, Predecessor) ->
  %%case Predecessor of
    %%nil ->
      %%{Nkey, Npid};
    %% TODO: ADD SOME CODE
    %%{Pkey,  _} ->
      %%case key:between(Nkey, Pkey, MyKey) of
        %%true ->
          %%{Nkey, Npid};
        %% TODO: ADD SOME CODE
        %%false ->
          %%Predecessor
      %%end
  %%end.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Store, MyKey, Nkey, Npid),
      Nref = monit(Npid),
      {{Nkey, Nref, Npid}, Keep};
      %% TODO: ADD SOME CODE
    {Pkey, Pref,_} ->
      case key:between(Nkey, Pkey, MyKey) of
        true ->
            Keep = handover(Store, MyKey, Nkey, Npid),
              %% TODO: ADD SOME CODE
              demonit(Pref),
              Nref = monit(Npid),
              {{Nkey, Nref, Npid}, Keep};
              %% TODO: ADD SOME CODE
        false ->
            {Predecessor, Store}
      end
  end.


create_probe(MyKey, {_, _, Spid}) ->
  Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
  io:format("Create probe ~w!~n", [MyKey]).

remove_probe(MyKey, Nodes, T, Store) ->
  T2 = erlang:monotonic_time(),
  Time = erlang:convert_time_unit(T2-T, native, millisecond),
  io:format("Received probe ~w in ~w ms Ring: ~w and Store: ~w~n", [MyKey, Time, Nodes, Store]).

forward_probe(RefKey, Nodes, T, {_, _, Spid}) ->
  Spid ! {probe, RefKey, Nodes, T},
  io:format("Forward probe ~w!~n", [RefKey]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key , Pkey , MyKey) of %% TODO: ADD SOME CODE
    true ->
          Added = storage:add(Key, Value, Store) , %% TODO: ADD SOME CODE
          Client ! {Qref, ok},
          Added;
    false ->
          %% TODO: ADD SOME CODE
          Spid ! {add, Key, Value, Qref, Client},
          Store
  end.

lookup(Key, Qref, Client, MyKey, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key , Pkey , MyKey) of %% TODO: ADD SOME CODE
    true ->
          Result = storage:lookup(Key, Store) , %% TODO: ADD SOME CODE
          Client ! {Qref, Result};
    false ->
          %% TODO: ADD SOME CODE
          Spid ! {lookup, Key, Qref, Client}
  end.

handover(Store, MyKey, Nkey, Npid) ->
  {Keep, Leave} = storage:split(MyKey, Nkey, Store),
  Npid ! {handover, Leave},
  Keep.

monit(Pid) ->
  erlang:monitor(process, Pid).

demonit(nil) ->
  ok;
demonit(MonitorRef) -> 
  erlang:demonitor(MonitorRef, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};

down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  Nref = monit(Npid),
  self() ! stabilize,
  {Predecessor, {Nkey, Nref, Npid}, nil}.
  

