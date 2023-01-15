:- use_module(library(lists)).
:- dynamic neg/3 .
:- dynamic brake/0 .

% Latar - RDF Surfaces playground
% (c) Patrick Hochstenbach 2022-2023

% From (a,b,c,..) <-> [a,b,c,...]
conj_list(true, []) :-
    !.
conj_list(A, [A]) :-
    A \= (_, _),
    A \= false,
    !.
conj_list((A, B), [A|C]) :-
    conj_list(B, C).

% An ltriple is a pred(level,subject,object) expression 
ltriple(T,Level,Predicate,Subject,Object) :-
   nth0(0,L,Predicate),
   nth0(1,L,Level),
   nth0(2,L,Subject),
   nth0(3,L,Object),
   length(L,4),
   T =.. L,
   integer(Level).

% Return the level of an l-triple
level(A,B) :-
   ltriple(A,B,_,_,_).

% Return the predicate of an l-triple 
predicate(A,B) :-
   ltriple(A,_,B,_,_).

% Return the subject of an l-triple
subject(A,B) :-
   ltriple(A,_,_,B,_).

% Return the object of an l-triple
object(A,B) :-
   ltriple(A,_,_,_,B).

% Lift the level of a ltriple by one
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

% Drop the level of a statement by one
drop(A,B) :-
   lift(B,A).

% Apply a lift or drop to a (nested) statement
levelapply(Op,A,B) :-
    % Apply the level function on the A statement itself
    Do =.. [Op,A,X] ,
    Do ,

    level(X,Level) ,

    % Apply it on the parts
    subject(X,S),
    ( levelapply(Op,S,SN) -> true ; SN = S ) ,

    predicate(X,P),
    ( levelapply(Op,P,PN) -> true ; PN = P ) ,

    object(X,O),
    ( levelapply(Op,O,ON) -> true ; ON = O ) ,

    B =.. [PN,Level,SN,ON].

% Create an l-triple from a (possible nested) triple
makeltriple(Level,A,B) :-
    A =.. L,
    length(L,3),
    nth0(0,L,P),
    nth0(1,L,S),
    nth0(2,L,O),
    % Deeper nested triples have a higher level
    LevelUp is Level + 1 ,
    makeltripleG(LevelUp,S,Sn),
    makeltripleG(LevelUp,P,Pn),
    makeltripleG(LevelUp,O,On),
    % We know all that we need to know and can create the l-triple
    !,
    ltriple(B,Level,Pn,Sn,On).

makeltripleG(Level,A,B) :-
    conj_list(A,L),
    makeltripleG(Level,L,[],Ln),
    reverse(Ln,Lnn),
    conj_list(B,Lnn).

makeltripleG(_,[],Acc,Acc).

makeltripleG(Level,[H|T],Acc,B) :-
    ( is_triple(H) ->
        makeltriple(Level,H,Hn),
        makeltripleG(Level,T,[Hn|Acc],B)
        ;
        makeltripleG(Level,T,[H|Acc],B)
    ).

% Turn an ltriple graffiti references into one with prolog variables
% with
%      A  - the object of a negative surface
%      Gr - the graffiti list of a negative surface
%      B  - a new negative surface object with blank node references filled in
surface_make_graffiti(A,Gr,B) :-
    % Turn the graffiti list into a variable list
    make_var(Gr,GrVar),
    surface_make_graffiti(A,Gr,GrVar,B).

surface_make_graffiti(A,Gr,GrVar,B) :-
    conj_list(A,As),
    % Create a new object turning all blank node references into variables
    surface_make_graffiti(As,[],Gr,GrVar,New),
    reverse(New,NewR),
    conj_list(B,NewR).

surface_make_graffiti([],Acc,_,_,Acc).

surface_make_graffiti([H|T],Acc,Gr,GrVar,B) :-
    is_triple_or_formula(H),

    level(H,L),
    subject(H,S),
    predicate(H,P),
    object(H,O),

    % Process nested forumlas also
    ( is_triple_or_formula(S) -> 
        surface_make_graffiti(O,Gr,GrVar,ON)
        ;
        ( graffiti_expand(Gr,GrVar,S,SN) -> true ; SN = S )
    ),

    % Process nested forumlas also
    ( is_triple_or_formula(O) ->
        surface_make_graffiti(O,Gr,GrVar,ON)
        ;
        ( graffiti_expand(Gr,GrVar,O,ON) -> true ; ON = O )
    ),
    
    ltriple(New,L,P,SN,ON),
    surface_make_graffiti(T,[New|Acc],Gr,GrVar,B).

% Expand a blank node reference to the graffiti thereof
graffiti_expand(P,PVar,A,B) :-
    nth0(I,P,A),
    nth0(I,PVar,B).

% True when A looks like a triple
is_triple(A) :-
    compound(A),
    \+is_list(A).

% True when A looks like a triple or formula
is_triple_or_formula(A) :-
    conj_list(A,B),
    nth0(0,B,C),
    is_triple(C).

% True when A is a negative surface
is_negative_surface(A) :-
    predicate(A,Pred),
    atom_string(Pred,neg).

% True when A is an odd integer
is_odd(A) :-
    integer(A),
    1 is A mod 2.

% True when A is an even integer
is_even(A) :-
    integer(A),
    0 is A mod 2.

% Make variables for a list of graffiti
make_var(Ls,Vs) :-
    length(Ls,N) ,
    % Generate a list of length N with variables
    length(Vs,N) .

% Instantiate the sheet of assertion
sa(G) :-
    conj_list(G,L),
    insert_procedure([],L).

insert_procedure(_,[]).

insert_procedure([],[H|T]) :-
    makeltriple(0,H,Hn),
    assertz(Hn),
    insert_procedure([],T).

% Remove in a level 1 negative surface copies of
% triples that exist on level 0
deiterate_procedure :-
    neg(0,P,G),

    % Fill in the graffiti inside this surface
    surface_make_graffiti(G,P,Gprime),

    conj_list(Gprime,Gs),

    % Remove matches with level 0
    deiterate_procedure(Gs,[],GsNew),
    reverse(GsNew,T),
    conj_list(GNew,T),

    % Assert the new surface
    ( retract(neg(0,P,Gprime)) -> true ; true ) ,
    assertz(neg(0,P,GNew)).

deiterate_procedure([],Acc,Acc).

deiterate_procedure([H|T],Acc,B) :-
    levelapply(drop,H,Hn),
    Hn,
    deiterate_procedure(T,Acc,B).

deiterate_procedure([H|T],Acc,B) :-
    levelapply(drop,H,Hn),
    \+Hn,
    deiterate_procedure(T,[H|Acc],B).

double_cut_procedure :-
    neg(0,P1,neg(1,P2,G)),
    levelapply(drop,G,X),
    levelapply(drop,X,Gn),
    ( retract(neg(0,P1,neg(1,P2,G))) -> true ; true ) ,
    assertz(Gn).

% Peirce Abstract Machine is a combination of a deiteration
% with a double cut. 
pam :-
    deiterate_procedure,
    double_cut_procedure,
    retract(brake),
    fail.

pam :-
    ( brake -> ! 
        ; assertz(brake), pam 
    ).
