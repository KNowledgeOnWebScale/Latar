:- consult('../../core.pl').

'<http://www.w3.org/2000/10/swap/math#lessThan>'(_,A,B) :-
    A < B .

run :-
    run('program.n3p').
