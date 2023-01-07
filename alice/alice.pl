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

% an ltriple is a pred(level,subject,object) expression 
ltriple(T) :-
   nonvar(T),
   ltriple(T,_,_,_,_).

ltriple(T,Level,Predicate,Subject,Object) :-
   nth0(0,L,Predicate),
   nth0(1,L,Level),
   nth0(2,L,Subject),
   nth0(3,L,Object),
   length(L,4),
   T =.. L,
   integer(Level).

% return the level of an l-triple
level(A,B) :-
   ltriple(A,B,_,_,_).

% return the predicate of an l-triple 
predicate(A,B) :-
   ltriple(A,_,B,_,_).

% return the subject of an l-triple
subject(A,B) :-
   ltriple(A,_,_,B,_).

% return the object of an l-triple
object(A,B) :-
   ltriple(A,_,_,_,B).

% lift the level of a ltriple by one
lift(A,B) :-
   nonvar(A),
   var(B),
   ltriple(A,L,P,S,O),
   Ln is L + 1,
   ltriple(B,Ln,P,S,O).

lift(A,B) :-
   var(A),
   nonvar(B),
   ltriple(B,Ln,P,S,O),
   L is Ln - 1,
   ltriple(A,L,P,S,O).

% drop the level of a statement by one
drop(A,B) :-
   lift(B,A).

% apply a lift or drop to a (nested) statement
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

% create an l-triple from a (possible nested) triple
makeltriple(Level,A,B) :-
    integer(Level),
    nonvar(A),
    var(B),
    A =.. L,
    length(L,3),
    nth0(0,L,P),
    nth0(1,L,S),
    nth0(2,L,O),
    LevelUp is Level + 1 ,
    makeltripleG(LevelUp,P,Pn),
    makeltripleG(LevelUp,S,Sn),
    makeltripleG(LevelUp,O,On),
    ltriple(B,Level,Pn,Sn,On).

makeltripleG(Level,A,B) :-
    conj_list(A,L),
    makeltripleG(Level,L,[],Ln),
    reverse(Ln,Lnn),
    conj_list(B,Lnn).

makeltripleG(_,[],Acc,Acc).

makeltripleG(Level,[H|T],Acc,B) :-
    ( compound(H) ->
        makeltriple(Level,H,Hn),
        makeltripleG(Level,T,[Hn|Acc],B)
        ;
        makeltripleG(Level,T,[H|Acc],B)
    ).

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
    insert_procedure(0,P,L).



insert_procedure(_,_,[]).

insert_procedure(Level,P,[H|T]) :-
    H =.. L,
    nth0(0,L,Predicate),
    nth0(1,L,Subject),
    nth0(2,L,Object),
    St =.. [Predicate,Level,Subject,Object],
    (retract(St) -> true ; true) ,
    assertz(St),
    insert_procedure(Level,P,T).

%neg(0,P,G) :-
%    conj_list(G,L),
%    erase_prodecure(P,L,LN),
%    writeln(LN).

erase_prodecure(P,L,LN) :-
    erase_prodecure(P,L,[],LN) .

erase_prodecure(_,_,Acc,Acc) .

erase_prodecure(P,[H|T],Acc,LN) :-
    drop(H,HN),
    ( HN -> 
        erase_prodecure(P,T,Acc,LN)  
        ; 
        erase_prodecure(P,T,[H|Acc],LN)  
    ).
