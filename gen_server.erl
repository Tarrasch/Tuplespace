-module(gen_server).
-export([start/2,rpc/2]).

start(Name,Mod) ->
    spawn(fun() ->
		  %register(Name,self()),
		  server(Name,Mod,Mod:init(),[])
	  end).

server(Name,Mod2,State2,Askers2) ->
    {Mod, State, Askers} = answer_askers(Mod,State,Askers),
    reply(Name,Pid,{ok,Reply}),
    receive
	{Pid,Msg} ->
        newState = Mod:handle(Msg,State);
        server(Name,Mod,NewState,[{Pid, Msg} | Askers])
	    end;
    end.

answer_askers(Mod,State,Askers) -> aa_help(Mod,State,Askers, [])

aa_help(Mod,State,[{Pid, Msg} | },AskersR) -> aa_help(Mod,State,Askers)
    

rpc(Name,Msg) ->
    Name ! {self(), Msg},
    receive 
	{Name,{crash,Reason}} -> exit(Reason);
	{Name,{ok,Reply}} -> Reply
    end.

reply(Name,Pid,Reply) ->
    Pid ! {Name,Reply}.
