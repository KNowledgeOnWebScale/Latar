:- consult('../../core.pl').
:- dynamic '<http://example.org/ns#likes>'/3 .

query('<http://example.org/ns#likes>'(_LEVEL,_WHO,_WHAT)).

run :-
    load_n3p('program.n3p'),
    pam,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.