:- consult('../../core.pl').
:- dynamic type/3 .

program :- 
   sa((
     type('Alice','Person'),
     neg([],(
        type(A,'Person'),
        neg([],
            type(A,'Human')
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
