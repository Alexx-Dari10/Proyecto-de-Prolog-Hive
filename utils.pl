%export

:- module(utils, 
    [for/3
    ]).

%utils

for(Start, Index, End):- Start > End,!, fail.
for(Start, Start, End).
for(Start, Index, End):- Start1 is Start+1,!, for(Start1, Index, End).