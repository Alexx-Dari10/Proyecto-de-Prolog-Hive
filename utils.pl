%export

:- module(utils, 
    [for/3, bigger/2,remove_repeated/2
    ]).

%utils

for(Start, Index, End):- Start > End,!, fail.
for(Start, Start, End).
for(Start, Index, End):- Start1 is Start+1,!, for(Start1, Index, End).

bigger(Level, Value):- Level > Value.


remove_repeated(X,L1):- remove_repeated(X, [], L1).

remove_repeated([], L1, L1).
remove_repeated([X|Y], Z, L1):-  not(member(X,Z)),append(Z,[X],R), remove_repeated(Y,R,L1), !.
remove_repeated([X|Y], Z, L1):-  remove_repeated(Y,Z,L1).