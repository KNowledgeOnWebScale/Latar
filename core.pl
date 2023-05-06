:- use_module(library(lists)) .
:- use_module(library(debug)) .
:- dynamic neg/3 .
:- dynamic brake/0 .
:- dynamic answer/1 .
:- dynamic '<http://www.w3.org/2000/10/swap/log#onNegativeSurface>'/3.
:- dynamic '<http://www.w3.org/2000/10/swap/log#onPositiveSurface>'/3.
:- dynamic '<http://www.w3.org/2000/10/swap/log#onNeutralSurface>'/3.
:- dynamic '<http://www.w3.org/2000/10/swap/log#onQuerySurface>'/3.
:- dynamic '<http://www.w3.org/2000/10/swap/log#onConstructSurface>'/3.

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

is_ltriple(T) :-
    ltriple(T,_,_,_,_).

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
%      Gr - the graffiti list of a negative surface
%      A  - the object of a negative surface
%      B  - a new object with blank node references turned into prolog variables
% If the Gr is not a list, then we don't need to do anything
make_graffiti(Gr,A,A) :-
    \+ is_list(Gr).

make_graffiti(Gr,A,B) :-
    % graffiti needs to be a list
    is_list(Gr),

    % Turn the graffiti list into a prolog variable list
    make_var(Gr,GrVar),
    make_graffiti(Gr,A,GrVar,B).

make_graffiti(Gr,A,GrVar,B) :-
    conj_list(A,As),
    make_graffiti([],As,Gr,GrVar,New),
    reverse(New,NewR),
    conj_list(B,NewR).

make_graffiti(Acc,[],_,_,Acc).

make_graffiti(Acc,[H|T],Gr,GrVar,B) :-
    is_triple_or_formula(H),

    level(H,L),
    subject(H,S),
    predicate(H,P),
    object(H,O),

    % Process nested formulas also
    ( is_triple_or_formula(S) -> 
        make_graffiti(Gr,S,GrVar,SN)
        ;
        ( graffiti_expand(Gr,GrVar,S,SN) -> true ; SN = S )
    ),

    % Process nested formulas also
    ( is_triple_or_formula(O) ->
        make_graffiti(Gr,O,GrVar,ON)
        ;
        ( graffiti_expand(Gr,GrVar,O,ON) -> true ; ON = O )
    ),

    ltriple(New,L,P,SN,ON),
    make_graffiti([New|Acc],T,Gr,GrVar,B).

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

% Make variables for a list of graffiti
make_var(Ls,Vs) :-
    length(Ls,N) ,
    % Generate a list of length N with variables
    length(Vs,N) .

% Make a new variable
make_var(A) :-
    length([A|_],1) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PEIRCE Algorithm                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

surface(negative,'<http://www.w3.org/2000/10/swap/log#onNegativeSurface>').
surface(positive,'<http://www.w3.org/2000/10/swap/log#onPositiveSurface>').
surface(neutral,'<http://www.w3.org/2000/10/swap/log#onNeutralSurface>').
surface(query,'<http://www.w3.org/2000/10/swap/log#onQuerySurface>').
surface(construct,'<http://www.w3.org/2000/10/swap/log#onConstructSurface>').

% Check if A is a surface
is_surface(A) :-
    predicate(A,X),
    surface(_,X).

% Generalize a surface by turning the blank node graffiti into a variable
generalize_if_surface(A,B) :-
    ( is_surface(A) ->
        make_surface(Surface,Level,_,Body,A),
        make_var(VarGraffiti),
        generalize_if_surface(Body,BodyNew),
        make_surface(Surface,Level,VarGraffiti,BodyNew,B)
        ;
        B = A 
    ).

% Create a surface statement from the parts
%  - Surface - a surface type
%  - Level - a level
%  - Graffiti - list of blank node graffiti
%  - Body - the body of the surface
%  - Stmt - the resulting surface
make_surface(Surface,Level,Graffiti,Body,Stmt) :-
    surface(Surface,S),
    Stmt =.. [S,Level,Graffiti,Body].

% Assert a statement
iterate(Stmt) :-
    assertz(Stmt).

% Assert a new surface
%  - Surface a surface type
%  - Level - a level
%  - Graffiti - list of blank node graffiti
%  - Body - surface subject
iterate(Surface,Level,Graffiti,Body) :-
    surface(Surface,S),
    Stmt =.. [S,Level,Graffiti,Body],
    iterate(Stmt).

% Retract a statement
deiterate(Stmt) :-
    ( retract(Stmt) -> true ; true ).

% Retract a surface
%  - Surface a surface type
%  - Level - a level
%  - Graffiti - list of blank node graffiti
%  - Body - surface subject
deiterate(Surface,Level,Graffiti,Body) :-
    surface(Surface,S),
    Stmt =.. [S,Level,Graffiti,Body],
    deiterate(Stmt).

% Remove in a level 1 surface copies of
% triples that exist on level 0
deiterate_procedure(Surface,_) :-
    debug(info, "deiterate_procedure", []),

    % Create a Stmt which represents a level 0 Surface with wildcard graffiti and body
    % If Surface = negative this will create a Stmt which represents a negative surface
    % on the Sheet of Assertion
    make_surface(Surface,0,P,G,Stmt),

    % Match this Stmt with everything we have in the knowledge base (query the database)
    Stmt,  % This will results in 0 or more matches (ltriple,...)

    % Turn all the graffiti coreferences in the body of the surface into Prolog variables
    % (if the graffiti P is not a list, then GPrime = G)
    make_graffiti(P,G,GPrime),

    % Turn the body of the surface (a conjunction of ltriples), into a list of ltriples
    conj_list(GPrime,Gs),

    % Remove matches with level 0
    % The deiterate_procedure creates a new body without the ltriples that exist in 
    % the parent of the surface
    deiterate_procedure(Gs,[],GsNew),
    reverse(GsNew,T),   % In order to keep the same order of ltriples as before (not really needed)
    conj_list(GNew,T),  % Create from the list again a conjunction of ltriples 

    debug(debug, "-deiterate: neg(~q,~q)", [P,GPrime]),
    debug(trace, "-iterate: neg(~q,~q)" , [P,GNew]),

    % Assert the new surface
    deiterate(Surface,0,P,GPrime), % Remove the old version of the surface
    iterate(Surface,0,P,GNew).     % Insert the new version of the surface

deiterate_procedure([],Acc,Acc).

deiterate_procedure([H|T],Acc,B) :-
    levelapply(drop,H,Hn),
    % If we have a surface, ignore the graffiti ..(they should already have been turned into variables in previous steps)
    generalize_if_surface(Hn,HnGeneral),
    call_if_exists(HnGeneral),
    deiterate_procedure(T,Acc,B).

deiterate_procedure([H|T],Acc,B) :-
    levelapply(drop,H,Hn),
    % If we have a surface, ignore the graffiti ..(they should already have been turned into variables in previous steps)
    generalize_if_surface(Hn,HnGeneral),
    not_exists(HnGeneral),
    deiterate_procedure(T,[H|Acc],B).

% Scan for contradictions
% An empty surface is a contradiction
empty_surface_procedure(Surface) :-
    make_surface(Surface,0,_,true,Stmt) ,
    ( call_if_exists(Stmt) ->
        writeln('***contradicton detected***'),
        throw(halt(2))
        ;
        true
    ).

% Remove double nested surfaces and assert the
% body of these surfaces, only when it is not already
% asserted
double_cut_procedure(OuterType,InnerType,Target) :-
    debug(info, "double_cut_procedure", []),

    % Create an Inner which matches a level 1 surface
    make_surface(InnerType,1,InnerGraffiti,G,Inner),
    % Create an Outer which matches a level 0 surface that contains Inner
    make_surface(OuterType,0,OuterGraffiti,Inner,Outer),

    % Find all these double nested surfaces in the knowledge base
    Outer, % This will result zero or more matches (ltriple,...)

    % Surfaces are only nested when the graffiti are lists
    % whatever the content
    is_list(InnerGraffiti),
    is_list(OuterGraffiti),

    % Surface are only nested when the object is an ltriple
    is_ltriple(G),

    debug(debug,"-cut: neg(neg(~q))", [G]),

    % If we have such a double nested surfaces, get its body and create
    % a new version of it but two levels up (closer to the top level) 
    levelapply(drop,G,X),
    levelapply(drop,X,Gn),

    % Remove the double nested surface
    deiterate(Outer),

    % Check if we need to assert the new "2-level up" ltriples on the answer surface
    assert_if_answer(Gn,Target),

    % Add the new "2-level up" ltiples to the knowledge base (if they don't exist yet)
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
    debug(info,"query_procedure", []),

    make_surface(query,0,Graffiti,Graph,Query),
    Query,

    debug(debug,"-query: ~q", [Query]),

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
    debug(info, "pam_default", []),

    % Empty negative surfaces are an indication of a contradiction in the knowledge base
    empty_surface_procedure(negative),
    % Remove from the negative surfaces on top level all the l-triples that also exist
    % on the top level
    deiterate_procedure(negative,default),
    % Turn double-nested negative surfaces on top level into assertions of their body
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
    debug(info, "pam_answer" , []),

    empty_surface_procedure(negative),
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

% Parse the N3P file and load it into memory
load_n3p(File) :-
    debug(info, "load_n3p(~q)", [File]),

    open(File, read, In, [encoding(utf8)]) ,
    repeat,
        read_term(In,Term,[]),
        (  Term == end_of_file -> 
           ! 
           ;
           process_term(Term) , fail
        ),
    close(In).

% N3P terms are transformed into ltriples and loaded into the knowledge base
process_term(Term) :-
    % Each n3p term is for the format:
    %     predicate(subject,object)
    % where typically in RDF Surfaces the object can be a nested graph.
    % When processing RDF Surfaces it is important to know the nesting level
    % of triples. We introduce 'ltriple' is introduced as a n3p term with
    % the nesting level included:
    %     predicate(level,subject,object)
    triple2ltriple(Term,TermN),
    % Assert this ltriple as a new fact in the the knowledge base (if it 
    % doesn't exist yet )
    ( call_if_exists(TermN) ->
        fail 
        ;
        iterate(TermN)
    ).

% Run the Peirce Abstract Machine and write the inferences into the knowledge base
run_default :-
    debug(info, "run_default",[]),

    pam_default,
    fail; true.

insert_query :-
    debug(info, "insert_query",[]),

    query_procedure,
    fail ; true .

% Run the Peirce Abstract Machine and write the inferences to the standard output
run_answer :-
    debug(info, "run_answer",[]),

    pam_answer,
    answer(Q),
    triple2ltriple(QN,Q),
    writeq(QN),
    write('.\n'),
    fail;
    true.

% Main 

run(Program) :-
    debug(info, "program: ~q" , [Program]),

    load_n3p(Program),
    run_default,
    insert_query,
    run_answer.
