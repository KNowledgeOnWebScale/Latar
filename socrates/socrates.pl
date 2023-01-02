:- consult('../core.pl').

rules :-

    pos(_,['http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('http://example.org/ns#Socrates','http://example.org/ns#Man')]),

    neg(_,[
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('http://example.org/ns#Socrates','http://example.org/ns#Man'),

        neg(_,[
            'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'('http://example.org/ns#Socrates','http://example.org/ns#Mortal')
        ])
    ]).

query('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'(_WHO,_WHAT)) .

run :-
    rules,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
