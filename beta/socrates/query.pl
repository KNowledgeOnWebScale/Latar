:- consult('../../core.pl').

run :-
    load_n3p('program.n3p'),
    pam(default),
    query_procedure,
    pam(answer),
    answer(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.