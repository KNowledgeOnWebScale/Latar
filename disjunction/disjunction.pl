:- dynamic 'http://www.w3.org/1999/02/22-rdf-syntax-ns#a'/2 .
:- dynamic 'http://example.org/ns#likes'/2 .

:- consult('../core.pl').

rules :-
    neg([],['http://example.org/ns#likes'('http://example.org/ns#Alice','http://example.org/ns#Tea')]),

    neg([],[
        neg([],['http://example.org/ns#likes'('http://example.org/ns#Alice','http://example.org/ns#Tea')]),
        neg([],['http://example.org/ns#likes'('http://example.org/ns#Alice','http://example.org/ns#Coffee')])
    ]).

query('http://example.org/ns#likes'(_WHO,_WHAT)) .

run :-
    rules,
    query(Q),
    Q,
    writeq(Q),
    write('.\n'),
    fail;
    true.
