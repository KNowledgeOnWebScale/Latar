pos(_,[]) .
pos(P,[H|T]) :-  
    assertz(H) , 
    pos(P,T) .

neg(_,[neg(_,X)]) :- pos(_,X) , ! .
neg(_,[]) :- false .

% erasure rule
%  if H is the case, then erase 
neg(P,[H|T]) :-
    H,
    neg(P,T) .
