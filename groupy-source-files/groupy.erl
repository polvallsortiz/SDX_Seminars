-module(groupy).
-export([start/2, stop/0, stop/1]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.

start(Module, Sleep) ->
    P = worker:start("P1", Module, Sleep),
    register(a, P), 
    spawn('p1@127.0.0.1', fun() -> group_leader(whereis(user), self()),
				register(b, worker:start("P2", Module, P, Sleep))
				end),
    spawn('p2@127.0.0.1',fun() -> group_leader(whereis(user), self()),
				register(c, worker:start("P3", Module, P, Sleep))
				end),
    spawn('p3@127.0.0.1', fun() -> group_leader(whereis(user), self()),
				register(d, worker:start("P4", Module, P, Sleep))
				end),
    spawn('p4@127.0.0.1', fun() -> group_leader(whereis(user), self()),
				register(e, worker:start("P5", Module, P, Sleep))
				end),
	receive				
	after 10000 ->
	    spawn('p5@127.0.0.1', fun() -> group_leader(whereis(user), self()),
				register(e, worker:start("P6", Module, whereis(P5), Sleep))
				end)
	end.

stop() ->
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

