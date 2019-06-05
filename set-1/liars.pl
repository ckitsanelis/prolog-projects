:- set_flag(print_depth, 1000). 


liars([],[]).
liars([X|L1],Y) :- biggest_element([X|L1],BB), find_liars([X|L1],0,Y,BB,U).

find_liars([X|Tail],N,NewList,BB,[NewList|AllLists]) :- generator([X|Tail],N,NewList), number_of_liars(NewList,A), compare_liars([X|Tail],NewList,A), duplicate_list(NewList,AllLists).
find_liars([X|Tail],N,NewList,BB,AllLists) :- N1 is N+1, N1=<BB, find_liars([X|Tail],N1,NewList,BB,AllLists).

%% Metraei ton arithmo twn pseftwn se mia lista
number_of_liars([],0).
number_of_liars([X|Tail],N) :- number_of_liars(Tail,N1), X=:=1, N is N1 + 1, !.
number_of_liars([X|Tail],N) :- number_of_liars(Tail,N1), N is N1.

%% Elegxei vasi tis listas pou dimiourgithike an oi pseftes eipan psemata kai oi upoloipoi alitheia
compare_liars([],[],N).
compare_liars([X|Val],[Y|Liars],N) :- Y=:=1, X>N, compare_liars(Val,Liars,N), !.
compare_liars([X|Val],[Y|Liars],N) :- Y=:=0, X=<N, compare_liars(Val,Liars,N).

%% Dimiourgei mia pithani lista me pseftes vasi mias timis
generator([],X,[]). 
generator([A|Tail],X,[1|NewList]) :- A > X, generator(Tail,X,NewList), !.
generator([A|Tail],X,[0|NewList]) :- generator(Tail,X,NewList).

%% Vriskei ti megaluteri timi se mia lista
biggest_element([],0).
biggest_element([X|Tail],X) :- biggest_element(Tail,N), X>N, !.
biggest_element([X|Tail],N) :- biggest_element(Tail,N).

%% Elegxei an mia lista uparxei se mia lista listwn
duplicate_list(A,[]).
duplicate_list(A,[B|Tail]) :- A \= B, duplicate_list(A,Tail).


/**

O xristis dinei mia lista. Vriskoume ti megaluteri timi(N) sti lista. Den uparxei periptwsi na uparxoun perissoteroi kleftes apo auti tin timi 
kathws tote oloi oloi tha eixan dikio ara tha eixame 0 pseftes(atopo). Sti sunexeia gia kathe pithani timi pseftwn(apo 0 ews N) dimiourgoume ti 
lista pseftwn pou tha proekupte apo tis times pou edwse o xristis. Metrame tous pseftes sti lista pou ftiaxame kai telos sugrinoume an sumpiptei i arxiki lista
me ti lista pseftwn pou dimiourgisame. Diladi opou i lista pseftwn exei 1 an i arxiki lista thewrei pws uparxoun perissoroi pseftes apo oti uparxoun.

?- liars([3,2,1,4,2], Liars).
Liars = [1,0,0,1,0]
?- liars([9,1,7,1,8,3,8,9,1,3], Liars).
Liars = [1,0,1,0,1,0,1,1,0,0]
?- liars([12,3,9,15,8,9,0,15,9,6,14,6,3,3,9], Liars).
Liars = [1,0,1,1,0,1,0,1,1,0,1,0,0,0,1]
?- liars([2,3,3,4], Liars).
no
?- liars([4,9,1,12,14,8,1,17,3,6,5,6,18,20,0,8,7,9,4,16],Liars).
Liars = [0,1,0,1,1,0,0,1,0,0,0,0,1,1,0,0,0,1,0,1]
?- liars([13,2,2,14,24,7,25,19,10,14,16,3,24,12,9,16,16,0,15,16,5,19,2,3,16], Liars).
Liars = [0,0,0,1,1,0,1,1,0,1,1,0,1,0,0,1,1,0,1,1,0,1,0,0,1]
?- liars([11,15,29,17,20,30,25,15,14,24,26,21,8,21,28,8,5,28,9,6,28,8,20,18,10,29,28,16,0,5], Liars).
Liars = [0,0,1,1,1,1,1,0,0,1,1,1,0,1,1,0,0,1,0,0,1,0,1,1,0,1,1,0,0,0]

*/
