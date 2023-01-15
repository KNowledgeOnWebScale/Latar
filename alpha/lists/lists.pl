:- consult('../../core.pl').
:- dynamic likes/3 .

program :- 
   sa((
     likes('Alice',['Apples','Pears']),
     neg([],(
        likes('Alice',['Apples','Pears']),
        neg([],
            likes('Alice','Fruit')
        )
            )
     )
    )
   ).

query(likes(_LEVEL,_WHO,_WHAT)).

run :-
    program,
    pam,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
