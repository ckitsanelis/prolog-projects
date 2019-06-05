:- set_flag(print_depth, 1000). 

:- lib(ic).
:- lib(branch_and_bound).

maxclq(N,D,Clique,Size) :-   create_graph(N,D,Graph),
                             reverse_graph(1,2,Reverse,Graph,N),
				                 length(BinaryClique,N),
				                 BinaryClique #:: [0,1],
				                 constrain(BinaryClique,Reverse),
				                 Cost #= (N - sum(BinaryClique)), 
				                 bb_min(search(BinaryClique,0,input_order,indomain,complete,[]),Cost, bb_options{strategy:continue}),
                             get_results(BinaryClique,1,Clique,Size,N).

%% Periorismos: oi komvoi pou de sundeontai na min einai kai oi 2 sti klika
constrain(_,[]).
constrain(BinaryClique,[N1 - N2|Rest]) :- not_connected(BinaryClique,N1,N2,1), constrain(BinaryClique,Rest).

not_connected([H|T],A,B,Counter) :- A #= Counter, Counter1 is Counter + 1, not_connected(T,-1,B,Counter1,H).
not_connected([_|T],A,B,Counter) :- A #> Counter, Counter1 is Counter + 1, not_connected(T,A,B,Counter1).
not_connected([H|_],_,B,Counter,Y) :- B #= Counter, H + Y #=< 1.
not_connected([_|T],-1,B,Counter,Y) :- B #> Counter, Counter1 is Counter + 1, not_connected(T,-1,B,Counter1,Y).

%% Dimiourgia listas me tis akmes pou de sundeontai
reverse_graph(A,B,[],_,Max) :- A #= Max, B #= Max +1, !.
reverse_graph(A,B,New,Graph,Max) :- B #= Max + 1, !, A1 is A + 1, B1 is A1 + 1, reverse_graph(A1,B1,New,Graph,Max).
reverse_graph(A,B,[A - B|New],[N1 - N2|Graph],Max) :- (A #\= N1 or B #\= N2), !, B1 is B+1, reverse_graph(A,B1,New,[N1 - N2|Graph],Max).
reverse_graph(A,B,New,[N1 - N2|Graph],Max) :- B1 is B + 1, !, reverse_graph(A,B1,New,Graph,Max).
reverse_graph(A,B,[A - B|New],[],Max) :- A #< Max, B #= Max, !, A1 is A + 1, B1 is A1 + 1, reverse_graph(A1,B1,New,[],Max).
reverse_graph(A,B,[A - B|New],[],Max) :- A #< Max, B1 is B + 1, !, reverse_graph(A,B1,New,[],Max).

%% Metatropi tis duadikis listas twn komvwn sti klika se dekadiki
get_results([],_,[],0,_).
get_results([H|T],C,[C|New],S,N) :- H #= 1, !, C1 is C + 1, get_results(T,C1,New,S1,N), S is S1 + 1.
get_results([_|T],C,New,S,N) :- C1 is C + 1, get_results(T,C1,New,S,N).


create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.
