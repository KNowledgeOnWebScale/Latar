:- consult('../../core.pl').
:- dynamic type/3 .

program :- 
   sa((
     type('Alice','Person'),
     neg([],(
        type('Alice','Person'),
        neg([],
            type('Alice','Human')
        )
            )
     )
    )
   ).

query(type(_LEVEL,_WHO,_WHAT)).

run :-
    program,
    pam,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
