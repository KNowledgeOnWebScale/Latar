:- discontiguous neg/2.

makevar(A,B) :-
    A =.. [H|L] ,
    ( H = ',' ->
        B = L ;
        B = [A]
    ).

%neg(P,(neg(Q,A))) :- 
%    append(P,Q,R),
%    create(R,A) .

create(P,A) :-
    makevar(A,L),
    create_(P,L).

create_(_,[]).

create_(P,[H|T]) :-
    ( retract(H) -> true ; true ),
    assertz(H),
    create_(P,T).

pos(P,A) :-
    neg(P,neg([],A)).

type('Alice','Person').
neg([],(
    type('Alice','Person'),
    neg([],
        type('Alice','Human')
    )
  )
).

