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
    
rules :-

    pos(_,['http://example.org/ns#Man'('http://example.org/ns#Socrates')]),

    neg(_,[
        'http://example.org/ns#Man'('http://example.org/ns#Socrates'),
        neg(_,[
            'http://example.org/ns#Mortal'('http://example.org/ns#Socrates')
        ])
    ]).

query('http://example.org/ns#Mortal'(_WHO)) .

run :-
    rules,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
