:- consult('../core.pl').

rules :-

    pos(_,['http://example.org/ns#Man'('http://example.org/ns#Socrates')]),

    neg(_,[
        'http://example.org/ns#Man'('http://example.org/ns#Socrates'),
        neg(_,[
            'http://example.org/ns#Mortal'('http://example.org/ns#Socrates')
        ])
    ]).

query('http://example.org/ns#Mortal'(_WHO)) .

run :-
    rules,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
