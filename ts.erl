-module(ts).
-export([new/0, in/2, out/2, match/2]).

% returns the PID of a new (empty) tuplespace.
new() -> undefined.


% returns a tuple matching Pattern from tuplespace TS. Note that this operation will block if there is no such tuple.
in(TS,Pattern) -> apa.

% puts Tuple into the tuplespace TS. 
out(TS,Tuple) -> bepa.

% True if pattern matches value
match(any,_) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q)
                -> match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> case match(P,L) of
                              true -> match(PS,LS); 
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.

