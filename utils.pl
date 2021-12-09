%export

:- module(utils, 
    [bigger/2,remove_repeated/2,switch/2
    ]).

%utils


bigger(Level, Value):- Level > Value.


remove_repeated(X,L1):- remove_repeated(X, [], L1).

remove_repeated([], L1, L1).
remove_repeated([X|Y], Z, L1):-  not(member(X,Z)),append(Z,[X],R), remove_repeated(Y,R,L1), !.
remove_repeated([X|Y], Z, L1):-  remove_repeated(Y,Z,L1).

% switch case 
switch(Val, [T:Goal|Cases]) :-
    (Val=T -> call(Goal));
    (switch(Val, Cases)).
