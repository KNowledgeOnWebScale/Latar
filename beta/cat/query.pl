:- consult('../../core.pl').

query('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(_LEVEL,_WHO,_WHAT)).

run :-
    load_n3p('program.n3p'),
    pam,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.