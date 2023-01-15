:- consult('../../core.pl').
:- dynamic likes/3 .

program :- 
   sa((
     neg([],likes('Alice','Coffee')),
     neg([],(
            neg([],likes('Alice','Coffee')),
            neg([],likes('Alice','Tea'))
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
