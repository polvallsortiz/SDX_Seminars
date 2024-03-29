-module(node2).
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
  node(MyKey, Predecessor, Successor, Store).

connect(MyKey, nil) ->
  {ok, {MyKey , self()}};    %% TODO: ADD SOME CODE
connect(_, PeerPid) ->
  Qref = make_ref(),
  PeerPid ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey , PeerPid}}    %% TODO: ADD SOME CODE
  after ?Timeout ->
    io:format("Timeout: no response from ~w~n", [PeerPid])
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, MyKey},
      node(MyKey, Predecessor, Successor, Store);
    {notify, NewPeer} ->
      {NewPredecessor, NewStore} = notify(NewPeer, MyKey, Predecessor, Store),
      node(MyKey, NewPredecessor, Successor, NewStore);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(MyKey, Predecessor, Successor, Store);
    {status, Pred} ->
      NewSuccessor = stabilize(Pred, MyKey, Successor),
      io:format("Status with postsuccessor: ~w~n", [NewSuccessor]),
      node(MyKey, Predecessor, NewSuccessor, Store);
    stabilize ->
      io:format("Stabilize with successor: ~w~n", [Successor]),
      stabilize(Successor),
      node(MyKey, Predecessor, Successor, Store);
    stop ->
      ok;
    probe ->
      create_probe(MyKey, Successor),
      node(MyKey, Predecessor, Successor, Store);
    {probe, MyKey, Nodes, T} ->
      remove_probe(MyKey, Nodes, T, Store),
      node(MyKey, Predecessor, Successor, Store);
    {probe, RefKey, Nodes, T} ->
      forward_probe(RefKey, [MyKey|Nodes], T, Successor),
      node(MyKey, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
      node(MyKey, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
      node(MyKey, Predecessor, Successor, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(MyKey, Predecessor, Successor, Merged)

end.

stabilize(Pred, MyKey, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      %% TODO: ADD SOME CODE
      Spid ! {notify, {MyKey, self()} },
      Successor;
    {MyKey, _} ->
      Successor;
    {Skey, _} ->
      Spid ! {notify, {MyKey, self()} },
      %% TODO: ADD SOME CODE
      Successor;
    {Xkey, Xpid} ->
      case key:between(Xkey, MyKey, Skey) of
        true ->
          self() ! stabilize,
          {Xkey, Xpid};
        %% TODO: ADD SOME CODE
        %% TODO: ADD SOME CODE
        false ->
          %% TODO: ADD SOME CODE
          Spid ! {notify, {MyKey, self()} },
          Successor
      end
  end.

stabilize({_, Spid}) ->
  io:format("Stabilize with Spid: ~w~n", [Spid]),
  Spid ! {request, self()}.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
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
      {{Nkey, Npid}, Keep};
      %% TODO: ADD SOME CODE
    {Pkey, _} ->
      case key:between(Nkey, Pkey, MyKey) of
        true ->
            Keep = handover(Store, MyKey, Nkey, Npid),
              %% TODO: ADD SOME CODE
            {{Nkey, Npid}, Keep};
              %% TODO: ADD SOME CODE
        false ->
            {Predecessor, Store}
      end
  end.


create_probe(MyKey, {_, Spid}) ->
  Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
  io:format("Create probe ~w!~n", [MyKey]).

remove_probe(MyKey, Nodes, T, Store) ->
  T2 = erlang:monotonic_time(),
  Time = erlang:convert_time_unit(T2-T, native, millisecond),
  io:format("Received probe ~w in ~w ms Ring: ~w and Store: ~w~n", [MyKey, Time, Nodes, Store]).

forward_probe(RefKey, Nodes, T, {_, Spid}) ->
  Spid ! {probe, RefKey, Nodes, T},
  io:format("Forward probe ~w!~n", [RefKey]).

add(Key, Value, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
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

lookup(Key, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
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


