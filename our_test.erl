-module(our_test).
-export([test/0, slave/2]).
-import(
ts  
,[in/2,out/2,new/0]
). 

test() ->
    Delay = 500,
    process_flag(trap_exit, true),
    TS = new(),
    link(TS),
    io:format("TEST: new tuplespace TS created~n", []),

    % Test 1
    Slave1 = spawn_in_test(TS, {fish,any}),
    Slave2 = spawn_in_test(TS, {fowl,any}),

    out_test(TS, {fish,salmon}),
    out_test(TS, {fowl,chicken}), 

    sleep(Delay),
    replytest(Slave2, {fowl,any}, {fowl,chicken}),
    sleep(Delay),

    replytest(Slave1, {fish,any}, {fish,salmon}),
    sleep(Delay),

    % Test 2, see in uses BIFs indeed
    Slave3 = spawn_in_test(TS, any),
    Slave3 ! {TS, you_should_not_revieve_this},

    sleep(Delay),

    out_test(TS, anything),
    replytest(Slave3, any, anything).



%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(T) ->
    receive
    after
	T -> true
    end.

out_test(Tuplespace, Tup) ->
    io:format("TEST: out(TS, ~w)~n", [Tup]),
    out(Tuplespace, Tup).

% spawns a slave task to perform an in test. This function 
% returns the slave's Pid. The slave will forward the result of the 
% in operation to the caller.

spawn_in_test(Tuplespace, Pat) -> 
    S = spawn_link(test, slave, [Tuplespace, {self(), Pat}]),
    io:format("TEST: in(TS, ~w) by process ~w~n", [Pat, S]),
    S.

%% Get a tuple matching Item from Tuplespace T and send it to Pid
slave(T, {Pid,Item}) ->
    case in(T, Item) of
	    R -> Pid!{self(), R}
    end.

%% Tests whether the reply from a Slave task matches the expected Tuple
replytest(Slave, Pat, Tup) -> 
    io:format("Process ~w~n", [Slave]),
    receive
	    {Slave,Tup} ->
	        io:format("     Correct. in operation: ~w returned tuple: ~w~n", [Pat, Tup]);
        {Slave,Bad} ->
	        io:format("     Error. in with pattern: ~w returned tuple: ~w~n", [Pat, Bad])
    after 
        5000 ->   
	        io:format("     Error. No response for in operation with pattern: ~w~n", [Pat])
    end.

collect_exits([]) ->
    done;
collect_exits([Pid | Pids]) ->
    receive
	{'EXIT', Pid, _} ->
	    collect_exits(Pids)
    end.
