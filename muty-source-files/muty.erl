-module(muty).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter 
%to the start procedure. We also provide the average time (in milliseconds) 
%the worker is going to sleep before trying to get the lock (Sleep) 
%and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
	register(master,self()),
    spawn('p1@Lenovito',fun() -> register(l1, apply(Lock, start, [1])), 
		register(w1, worker:start("John", l1, Sleep, Work)) 
		end),
    spawn('p2@Lenovito',fun() -> register(l2, apply(Lock, start, [2])),
		register(w2, worker:start("Ringo", l2, Sleep, Work))
		end),
    spawn('p3@Lenovito',fun() -> register(l3, apply(Lock, start, [3])),
		register(w3, worker:start("Paul", l3, Sleep, Work))
		end),
    spawn('p4@Lenovito',fun() -> register(l4, apply(Lock, start, [4])),
		register(w4, worker:start("George", l4, Sleep, Work))
		end),

    addnode(4).
    

addnode(0) ->
		{l1, 'p1@Lenovito'} ! {peers, [{l2, 'p2@Lenovito'}, {l3, 'p3@Lenovito'}, {l4, 'p4@Lenovito'}]},
    {l2, 'p2@Lenovito'} ! {peers, [{l1, 'p1@Lenovito'}, {l3, 'p3@Lenovito'}, {l4, 'p4@Lenovito'}]},
    {l3, 'p3@Lenovito'} ! {peers, [{l1, 'p1@Lenovito'}, {l2, 'p2@Lenovito'}, {l4, 'p4@Lenovito'}]},
    {l4, 'p4@Lenovito'} ! {peers, [{l1, 'p1@Lenovito'}, {l2, 'p2@Lenovito'}, {l3, 'p3@Lenovito'}]}, 
    ok;


addnode(Number) ->
	receive
		reg ->
			addnode(Number-1)
	end.




stop() ->
    {w1, 'p1@Lenovito'} ! stop,
    {w2, 'p2@Lenovito'} ! stop,
    {w3, 'p3@Lenovito'} ! stop,
    {w4, 'p4@Lenovito'} ! stop.
