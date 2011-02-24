-module(ts).
-import(gen_server, [start/2, rpc/2]).
-export([new/0, in/2, out/2, match/2]).

% --------------------------  EXTERNAL FUNCTIONS  ------------------------------
% returns the PID of a new (empty) tuplespace.
new() -> start(gts, ts),
         gts.


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

pattern_exists(Pattern, List) ->
    case search_match(Pattern, List) of
        {found, _ } -> true;
        _           -> false
    end.

search_match(Pattern, List) -> sm_help(Pattern, List, []).

sm_help(Pattern, [], ListR) -> {not_found, ListR};
sm_help(Pattern, [Tuple | ListL], ListR) -> 
    case match(Pattern, Tuple) of
        true -> {found, Tuple, ListL ++ ListR};
        _    -> sm_help(Pattern, ListL, [Tuple | ListR])
    end.

% --------------------------  INTERNAL GEN-SERVER FUNCTIONS  ------------------------------
init() -> []
handle({find_matching, Pattern}, State) -> 
    case search_match(Pattern, State) of
        {found, Tuple, State2} -> dosomething;
        {not_found, State2}    -> blah
    end;
handle({add_tuple, Tuple}, State) -> [Tuple | State].

can_handle({find_matching, Pattern}, State) -> pattern_exists(Pattern, State);
can_handle(_,_) -> true.
