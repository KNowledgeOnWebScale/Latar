:- consult('../../core.pl').

'<urn:example.org:makeString>'(
    _,
    literal(test, type('<http://www.w3.org/2001/XMLSchema#string>')),
    literal(test, type('<http://www.w3.org/2001/XMLSchema#string>'))    
).

run :-
    run('program.n3p').
