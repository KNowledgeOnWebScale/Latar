:- consult('../../core.pl').
:- dynamic type/3 .

program :- 
   sa((
     type('I','A'),

     neg([],type('I','C')),

     neg(['_:S'],(
        type('_:S','A'),

        neg([],
            type('_:S','B')
        ),

        neg([],
            type('_:S','C')
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
