:- use_module(library(lists)).
:- discontiguous neg/2.

% From (a,b,c,..) <-> [a,b,c,...]
conj_list(true, []) :-
    !.
conj_list(A, [A]) :-
    A \= (_, _),
    A \= false,
    !.
conj_list((A, B), [A|C]) :-
    conj_list(B, C).

% return the level of a statememt
level(A,B) :-
   A =.. L,
   nth0(1,L,B).

% return the subject of a statement
subject(A,B) :-
   A =.. L,
   nth0(2,L,B).

% return the predicate of a statement
predicate(A,B) :-
   A =.. L,
   nth0(0,L,B).

% return the object of a statement 
object(A,B) :-
   A =.. L,
   nth0(3,L,B).

% raise the level of a statement by one
raise(A,B) :-
   level(A,X),
   Y is X + 1,
   A =.. [F|[_|R]],
   B =.. [F|[Y|R]].

% lower the level of a statement by one
lower(A,B) :-
   level(A,X),
   Y is X - 1,
   A =.. [F|[_|R]],
   B =.. [F|[Y|R]].

% apply a raise or lower to a (nested) statement
levelapply(Op,A,B) :-
    % apply the level function on the A statement itself
    Do =.. [Op,A,X] ,
    Do ,

    level(X,Level) ,

    % apply it on the parts
    subject(X,S),
    ( levelapply(Op,S,SN) -> true ; SN = S ) ,

    predicate(X,P),
    ( levelapply(Op,P,PN) -> true ; PN = P ) ,

    object(X,O),
    ( levelapply(Op,O,ON) -> true ; ON = O ) ,

    B =.. [PN,Level,SN,ON].

is_odd(A) :-
    nonvar(A),
    atom(A),
    1 is A mod 2.

is_even(A) :-
    nonvar(A),
    atom(A),
    0 is A mod 2.

type(0,'Alice','Person').
neg(1,[],(
    type(1,'Alice','Person'),
    neg(1,[],
        type(2,'Alice','Human')
    )
  )
).

