-module(gen_server).
%-export([start/2,rpc/2]).

start() ->
    spawn(fun() ->
		  server(init(),[])
	  end).

server(State2,Askers2) ->
    {State, Askers} = answer_askers(State2,Askers2),
    reply(Name,Pid,{ok,Reply}),
    receive
	{Pid,Msg} ->
        newState = Mod:handle(Msg,State),
        server(NewState,[{Pid, Msg} | Askers])
    end.

answer_askers(State,Askers) -> aa_help(State,Askers, []).

aa_help(State,[{Pid, {find_matching, Pattern}} | AskersL ],AskersR) -> 
    case search_match(Pattern, State) of
        {found, Tuple, newState} -> reply(Pid,Tuple),
                                    aa_help(newState, AskersL, AskersR);
        {not_found, State}       -> aa_help(State, AskersL, [{Pid, {find_matching, Pattern}} | AskersR]) 
    end;
aa_help(State,[A | AskersL ],AskersR) -> aa_help(State,AskersL,[A | AskersR]);
aa_help(State,[],AskersR) -> {State, AskersR}.

rpc(Name,Msg) ->
    Name ! {self(), Msg},
    receive 
	    {Name,Reply} -> Reply
    end.

reply(Pid,Reply) ->
    Pid ! {self(),Reply}.
