:- consult('../../core.pl').
:- dynamic type/3 .

program :- 
   sa((
     type('Alice','Person'),
     neg(['_:A','_:B'],(
        type('_:A','Person'),
        neg([],
            type('_:A','Human')
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
