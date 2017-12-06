-module(merge).
-export([merge/2]).


merge([],[]) -> [];
merge([H|T],[]) -> [H|T];
merge([],[H|T]) -> [H|T];
merge([H1|T1], [H2|T2]) when H1 =< H2 -> [H1 | merge(T1, [H2|T2])]; 
merge([H1|T1], [H2|T2]) when H1 > H2 -> [H2 | merge([H1|T1], T2)]. 

