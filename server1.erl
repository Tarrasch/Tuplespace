-module(server1).
-export([start/2,rpc/2]).

start(Name,Mod) ->
    spawn(fun() ->
		  register(Name,self()),
		  server(Name,Mod,Mod:init())
	  end).

server(Name,Mod,State) ->
    receive
	{Pid,Msg} ->
	    case catch Mod:handle(Msg,State) of
		{'EXIT',Reason} ->
		    reply(Name,Pid,{crash,Reason}),
		    server(Name,Mod,State);
		{Reply,NewState} ->
		    reply(Name,Pid,{ok,Reply}),
		    server(Name,Mod,NewState)
	    end
    end.

rpc(Name,Msg) ->
    Name ! {self(), Msg},
    receive 
	{Name,{crash,Reason}} -> exit(Reason);
	{Name,{ok,Reply}} -> Reply
    end.

reply(Name,Pid,Reply) ->
    Pid ! {Name,Reply}.
