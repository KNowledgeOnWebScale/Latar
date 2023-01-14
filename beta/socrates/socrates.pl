:- consult('../../core.pl').
:- dynamic type/3 .
:- dynamic subClassOf/3 .

program :- 
   sa((
     type('Socrates','Human'),
     subClassOf('Human','Mortal'),
     neg(['_:A','_:B','_:S'],(
        subClassOf('_:A','_:B'),
        type('_:S','_:A'),
        neg([],
            type('_:S','_:B')
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
