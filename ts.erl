-module(ts).
-export([new/0, in/2, out/2, match/2]).

% --------------------------  EXTERNAL FUNCTIONS  ------------------------------
% returns the PID of a new (empty) tuplespace.
new() -> start().


% returns a tuple matching Pattern from tuplespace TS. Note that this operation will block if there is no such tuple.
in(TS,Pattern) -> rpc(TS, {find_matching, Pattern}).

% puts Tuple into the tuplespace TS. 
out(TS,Tuple) -> rpc(TS, {add_tuple, Tuple}).

% --------------------------  INTERNAL FUNCTIONS  ------------------------------
% True if value matches pattern
match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
           -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS); 
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.

search_match(Pattern, List) -> sm_help(Pattern, List, []).

sm_help(Pattern, [Tuple | ListL], ListR) -> 
    case match(Pattern, Tuple) of
        true -> {found, Tuple, ListL ++ ListR};
        _    -> sm_help(Pattern, ListL, [Tuple | ListR])
    end;
sm_help(_, [], ListR) -> {not_found, ListR}.

% --------------------------  INTERNAL GEN-SERVER FUNCTIONS  ------------------------------
init() -> [].
interpret_msg({add_tuple, Tuple}, State) -> [Tuple | State];
interpret_msg(_,State) -> State.

% --------------------------  SERVER FUNCTIONS  ------------------------------

start() ->
    spawn(fun() ->
          server(init(),[])
      end).

server(State2,Askers2) ->
    {State, Askers} = answer_askers(State2,Askers2),
    receive
    {Pid,Msg} ->
        NewState = interpret_msg(Msg,State),
        server(NewState,[{Pid, Msg} | Askers])
    end.

answer_askers(State,Askers) -> aa_help(State,Askers, []).

aa_help(State,[{Pid, {find_matching, Pattern}} | AskersL ],AskersR) -> 
    case search_match(Pattern, State) of
        {found, Tuple, NewState} -> reply(Pid,Tuple),
                                    aa_help(NewState, AskersL, AskersR);
        {not_found, State}       -> aa_help(State, AskersL, [{Pid, {find_matching, Pattern}} | AskersR]) 
    end;
aa_help(State,[{Pid, {add_tuple, _Tuple}} | AskersL ],AskersR) -> 
    reply(Pid, ok),
    aa_help(State,AskersL, AskersR);
aa_help(State,[A | AskersL],AskersR) -> 
    io:format("Unable to interpret msg ~c",[A]),
    aa_help(State,AskersL,AskersR);
aa_help(State,[],AskersR) -> {State, AskersR}.

rpc(Name,Msg) ->
    Name ! {self(), Msg},
    receive 
        {Name,Reply} -> Reply
    end.

reply(Pid,Reply) ->
    Pid ! {self(),Reply}.
