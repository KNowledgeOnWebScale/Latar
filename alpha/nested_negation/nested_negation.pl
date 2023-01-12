:- consult('../core.pl').
:- dynamic type/3 .

program :- 
   sa((
     neg([],
        neg([],
            neg([],
                neg([],
                    type('Alice','Human')
                )
            )
        )
     )
    )
   ).

query(type(_LEVEL,_WHO,_WHAT)).

run :-
    program,
    % need two steps due to double nesting
    inference_step,
    inference_step,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
