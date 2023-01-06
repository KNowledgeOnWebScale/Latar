:- use_module(library(lists)).
:- dynamic neg/3 .

%----------------------------------------

program :- 
   type(0,'Alice','Person'),
   neg(0,[],(
        type(1,'Alice','Person'),
        neg(1,[],
            type(2,'Alice','Human')
        )
    )
   ).

%----------------------------------------

% Interpretation part

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

% instantiate a level 0 statement
pos(P,G) :-
    conj_list(G,L),
    sa(P,L).

sa(_,[]).

sa(P,[H|T]) :-
    H =.. L,
    nth0(0,L,Predicate),
    nth0(1,L,Subject),
    nth0(2,L,Object),
    St =.. [Predicate,0,Subject,Object],
    (retract(St) -> true ; true) ,
    assertz(St),
    sa(P,T).

%neg(0,P,G) :-
%    conj_list(G,L),
%    erase_prodecure(P,L,LN),
%    writeln(LN).

erase_prodecure(P,L,LN) :-
    erase_prodecure(P,L,[],LN) .

erase_prodecure(_,_,Acc,Acc) .

erase_prodecure(P,[H|T],Acc,LN) :-
    lower(H,HN),
    ( HN -> 
        erase_prodecure(P,T,Acc,LN)  
        ; 
        erase_prodecure(P,T,[H|Acc],LN)  
    ).
