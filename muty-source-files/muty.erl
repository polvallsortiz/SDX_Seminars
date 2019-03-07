-module(muty).
-export([start/3, stop/0]).

% We use the name of the module (i.e. lock3) as a parameter 
%to the start procedure. We also provide the average time (in milliseconds) 
%the worker is going to sleep before trying to get the lock (Sleep) 
%and work with the lock taken (Work).

start(Lock, Sleep, Work) ->
    spawn('p1@c6s303pc22.fib.upc.es',fun() -> register(l1, apply(Lock, start, [1])), 
		l1 ! {peers, [{l2, p2@c6s303pc22.fib.upc.es}, l3, l4]},
		register(w1, worker:start("John", l1, Sleep, Work)) 
		end),
    spawn('p2@c6s303pc22.fib.upc.es',fun() -> register(l2, apply(Lock, start, [2])),
		l2 ! {peers, [l1, l3, l4]},
		register(w2, worker:start("Ringo", l2, Sleep, Work))
		end),
    spawn('p3@c6s303pc22.fib.upc.es',fun() -> register(l3, apply(Lock, start, [3])),
		l3 ! {peers, [l1, l2, l4]},
		register(w3, worker:start("Paul", l3, Sleep, Work))
		end),
    spawn('p4@c6s303pc22.fib.upc.es',fun() -> register(l4, apply(Lock, start, [4])),
		l4 ! {peers, [l1, l2, l3]}, 
		register(w4, worker:start("George", l4, Sleep, Work))
		end),
    ok.

stop() ->
    w1 ! stop,
    w2 ! stop,
    w3 ! stop,
    w4 ! stop.
