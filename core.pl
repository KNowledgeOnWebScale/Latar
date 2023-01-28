:- use_module(library(lists)).
:- dynamic neg/3 .
:- dynamic brake/0 .
:- dynamic answer/1 .

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% L-triples                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An l-triple is a pred(level,subject,object) expression 
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
    call_if_exists(Do) ,

    level(X,Level) ,

    % Apply it on the parts
    subject(X,S),
    ( levelapply(Op,S,SN) -> true ; SN = S ) ,

    predicate(X,P),
    ( levelapply(Op,P,PN) -> true ; PN = P ) ,

    object(X,O),
    ( levelapply(Op,O,ON) -> true ; ON = O ) ,

    B =.. [PN,Level,SN,ON].

% Triple to l-triple conversion
triple2ltriple(A,B) :-
    nonvar(A),
    var(B),
    triple-ltriple(0,A,B).

triple2ltriple(A,B) :-
    var(A),
    nonvar(B),
    ltriple-triple(B,A).

triple2ltriple(A,B) :-
    var(A),
    var(B),
    triple-ltriple(0,A,B).

% Create an l-triple from a (possible nested) triple
triple-ltriple(Level,A,B) :-
    nonvar(A),
    A =.. L,
    length(L,3),
    nth0(0,L,P),
    nth0(1,L,S),
    nth0(2,L,O),
    % Deeper nested triples have a higher level
    LevelUp is Level + 1 ,
    triple-ltripleG(LevelUp,S,Sn),
    triple-ltripleG(LevelUp,P,Pn),
    triple-ltripleG(LevelUp,O,On),
    % We know all that we need to know and can create the l-triple
    !,
    ltriple(B,Level,Pn,Sn,On).

triple-ltripleG(Level,A,B) :-
    conj_list(A,L),
    triple-ltripleG(Level,L,[],Ln),
    reverse(Ln,Lnn),
    conj_list(B,Lnn).

triple-ltripleG(_,[],Acc,Acc).

triple-ltripleG(Level,[H|T],Acc,B) :-
    ( is_triple(H) ->
        triple-ltriple(Level,H,Hn),
        triple-ltripleG(Level,T,[Hn|Acc],B)
        ;
        triple-ltripleG(Level,T,[H|Acc],B)
    ).

% create a triple from a (possible nested) l-triple
ltriple-triple(A,B) :-
    ltriple(A,_,Predicate,Subject,Object),
    ( is_triple(Subject) ->
        ltriple-triple(Subject,SubjectNew) ; SubjectNew = Subject 
    ),
    ( is_triple(Predicate) ->
        ltriple-triple(Predicate,PredicateNew) ; PredicateNew = Predicate
    ),
    ( is_triple(Object) ->
        ltriple-triple(Object,ObjectNew) ; ObjectNew = Object
    ),
    B =.. [PredicateNew,SubjectNew,ObjectNew].

% Turn l-triple graffiti references into one with prolog variables
% with
%      A  - the object of a negative surface
%      Gr - the graffiti list of a negative surface
%      B  - a new negative surface object with blank node references filled in
make_graffiti(A,Gr,B) :-
    % Turn the graffiti list into a variable list
    make_var(Gr,GrVar),
    make_graffiti(A,Gr,GrVar,B).

make_graffiti(A,Gr,GrVar,B) :-
    conj_list(A,As),
    % Create a new object turning all blank node references into variables
    make_graffiti(As,[],Gr,GrVar,New),
    reverse(New,NewR),
    conj_list(B,NewR).

make_graffiti([],Acc,_,_,Acc).

make_graffiti([H|T],Acc,Gr,GrVar,B) :-
    is_triple_or_formula(H),

    level(H,L),
    subject(H,S),
    predicate(H,P),
    object(H,O),

    % Process nested formulas also
    ( is_triple_or_formula(S) -> 
        make_graffiti(O,Gr,GrVar,ON)
        ;
        ( graffiti_expand(Gr,GrVar,S,SN) -> true ; SN = S )
    ),

    % Process nested formulas also
    ( is_triple_or_formula(O) ->
        make_graffiti(O,Gr,GrVar,ON)
        ;
        ( graffiti_expand(Gr,GrVar,O,ON) -> true ; ON = O )
    ),
    
    ltriple(New,L,P,SN,ON),
    make_graffiti(T,[New|Acc],Gr,GrVar,B).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PEIRCE Algorithm                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

surface(negative,'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>').
surface(positive,'<http://www.w3.org/2000/10/swap/log#onPositiveSurface>').
surface(neutral,'<http://www.w3.org/2000/10/swap/log#onNeutralSurface>').
surface(query,'<http://www.w3.org/2000/10/swap/log#onQuerySurface>').
surface(construct,'<http://www.w3.org/2000/10/swap/log#onConstructSurface>').

make_surface(Surface,Level,Graffiti,Body,Stmt) :-
    surface(Surface,S),
    Stmt =.. [S,Level,Graffiti,Body].

iterate(Stmt) :-
    assertz(Stmt).

iterate(Surface,Level,Graffiti,Body) :-
    surface(Surface,S),
    Stmt =.. [S,Level,Graffiti,Body],
    iterate(Stmt).

deiterate(Stmt) :-
    ( retract(Stmt) -> true ; true ).

deiterate(Surface,Level,Graffiti,Body) :-
    surface(Surface,S),
    Stmt =.. [S,Level,Graffiti,Body],
    deiterate(Stmt).

% Remove in a level 1 surface copies of
% triples that exist on level 0
deiterate_procedure(Surface,_) :-
    make_surface(Surface,0,P,G,Stmt),
    Stmt,

    % Fill in the graffiti inside this surface
    make_graffiti(G,P,GPrime),

    conj_list(GPrime,Gs),

    % Remove matches with level 0
    deiterate_procedure(Gs,[],GsNew),
    reverse(GsNew,T),
    conj_list(GNew,T),

    % Assert the new surface
    deiterate(Surface,0,P,GPrime),
    iterate(Surface,0,P,GNew).

deiterate_procedure([],Acc,Acc).

deiterate_procedure([H|T],Acc,B) :-
    levelapply(drop,H,Hn),
    call_if_exists(Hn),
    deiterate_procedure(T,Acc,B).

deiterate_procedure([H|T],Acc,B) :-
    levelapply(drop,H,Hn),
    not_exists(Hn),
    deiterate_procedure(T,[H|Acc],B).

% Remove double nested surfaces and assert the
% body of these surfaces, only when it is not already
% asserted
double_cut_procedure(OuterType,InnerType,Target) :-
    make_surface(InnerType,1,_,G,Inner),
    make_surface(OuterType,0,_,Inner,Outer),

    Outer,

    levelapply(drop,G,X),
    levelapply(drop,X,Gn),

    deiterate(Outer),

    assert_if_answer(Gn,Target),
    ( call_if_exists(Gn) ->
        fail 
        ;
        iterate(Gn)
    ).

% Write to a surface. 
%  When we have a default target do nothing
%  When we have an answer target add the Graph
assert_if_answer(_,default).
assert_if_answer(Graph,answer) :-
    ( call_if_exists(answer(Graph)) ->
        fail
        ;
        assertz(answer(Graph))
    ).

% Create a query construct surface
query_procedure :-
    make_surface(query,0,Graffiti,Graph,Query),
    Query,

    % Check if we already have a construct surface
    conj_list(Graph,Ls),
    make_surface(construct,1,_,_,Construct),

    ( memberchk(Construct,Ls) ->
        % Do nothing we already have a construct
        true 
        ;
        % Else create a construct surface
        levelapply(lift,Graph,Gn),

        make_surface(construct,1,[],Gn,Inner),
        make_surface(query,0,Graffiti,(Graph,Inner),Outer),

        deiterate(Query),
        iterate(Outer)
    ).

call_if_exists(G) :-
    current_predicate(_, G),
    call(G).

not_exists(G) :-
    \+current_predicate(_, G).

not_exists(G) :-
    current_predicate(_, G),
    ( G -> fail ; true ).

% Peirce Abstract Machine is a combination of a deiteration
% of triples in negative surfaces with a double cut of
% nested negative surfaces
pam_default :-
    deiterate_procedure(negative,default),
    double_cut_procedure(negative,negative,default),
    retract(brake),
    fail.

pam_default :-
    ( brake -> 
        ! 
        ; 
        assertz(brake), 
        pam_default
    ).

pam_answer :-
    deiterate_procedure(query,answer),
    double_cut_procedure(query,construct,answer),
    retract(brake),
    fail.

pam_answer :-
    ( brake -> 
        ! 
        ; 
        assertz(brake), 
        pam_answer
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% N3P loading into memory
load_n3p(File) :-
    open(File, read, In, [encoding(utf8)]) ,
    repeat,
        read_term(In,Term,[]),
        (  Term == end_of_file -> 
           ! 
           ;
           process_term(Term) , fail
        ),
    close(In).

process_term(Term) :-
     triple2ltriple(Term,TermN),
     assertz(TermN).

% Debug

verbose(Prefix,Msg) :-
    write(Prefix),
    write(" : "),
    writeln(Msg).

% Main

run_default :-
    pam_default,
    fail; true.

insert_query :-
    query_procedure,
    fail ; true .

run_answer :-
    pam_answer,
    answer(Q),
    triple2ltriple(QN,Q),
    writeq(QN),
    write('.\n'),
    fail;
    true.

run(Program) :-
    load_n3p(Program),
    run_default,
    insert_query,
    run_answer.
