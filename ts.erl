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

% The empty Tuple-container (This exists for modularity)
init() -> [].

% Search a Pattern in a Tuple-container.
% Returns: (See implementation)
search_match(Pattern, List) -> sm_help(Pattern, List, []).

% Help for search_match
sm_help(Pattern, [Tuple | ListL], ListR) -> 
    case match(Pattern, Tuple) of
        true -> {found, Tuple, ListL ++ ListR};
        _    -> sm_help(Pattern, ListL, [Tuple | ListR])
    end;
sm_help(_, [], ListR) -> {not_found, ListR}.

% --------------------------  INTERNAL GEN-SERVER FUNCTIONS  ------------------------------

% Interpret message, if the message was an add_tuple
% return a new state with the Tuple appended.
eventual_append({add_tuple, Tuple}, State) -> [Tuple | State];
eventual_append(_,State) -> State.

% --------------------------  SERVER FUNCTIONS  ------------------------------

% Spawn a tuplespace with correct arguments
start() ->
    spawn_link(fun() ->
          server(init(),[])
      end).

% Serverloop
% Carries the state and the list of answered askers.
server(State2,Askers2) ->
    {State, Askers} = answer_askers(State2,Askers2),
    receive
    {Pid,Msg} ->
        NewState = eventual_append(Msg,State),
        server(NewState,[{Pid, Msg} | Askers])
    end.

% Reply the askers that we can by now and return a new
% uppdated list of answered askers.
answer_askers(State,Askers) -> aa_help(State,Askers, []).

% Helpfunction for answer_askers
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

% Send a message to a tuplespace, this function makes sure the message gets
% sent with the correct tule-conventions the server uses.
rpc(Name,Msg) ->
    Name ! {self(), Msg},
    receive 
        {Name,Reply} -> Reply
    end.

% Internal function of server to reply client, makes sure reply has 
% a format rpc() recognizes.
reply(Pid,Reply) ->
    Pid ! {self(),Reply}.
