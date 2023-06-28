:- consult('../../core.pl').

'<urn:example.org:all>'(_,A,B) :-
    format("~w ~w~n",[A,B]).

run :-
    run('program.n3p').
