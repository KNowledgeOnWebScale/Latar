pos(_,[]) .
pos(P,[H|T]) :-  
    assertz(H) , 
    pos(P,T) .

neg(_,[neg(_,X)]) :- pos(_,X) , ! .
neg(_,[]) :- false .
neg(_,L) :-
    list_length(L,X) ,
    X =\= 0.

% erasure rule
%  if H is the case, then erase 
neg(P,[H|T]) :-
    H,
    neg(P,T) .

% utility
list_length([],0) .
list_length([_|T],X) :-
    list_length(T,Y) ,
    X is Y + 1 .
