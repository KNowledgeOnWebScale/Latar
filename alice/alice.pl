:- consult('core.pl').

program :- 
   sa([],(
     type('Alice','Person'),
     neg([],(
        type('Alice','Person'),
        neg([],
            type('Alice','Human')
        )
            )
     )
         )
   ).
