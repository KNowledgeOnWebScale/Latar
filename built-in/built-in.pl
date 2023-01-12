:- consult('../core.pl').
:- dynamic age/3 .
:- dynamic type/3 .

lessThan(_,A,B) :-
    A < B .

program :- 
   sa((
     age('Alice',12),
     neg([],(
        lessThan(12,20),
        neg([],
            type('Alice','teenager')
        )
            )
     )
    )
   ).

query(type(_LEVEL,_WHO,_WHAT)).

run :-
    program,
    inference_step,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
