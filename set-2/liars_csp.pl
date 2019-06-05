:- set_flag(print_depth, 1000). 

:- lib(ic).


liars_csp(List,Liars) :- 	length(List,N),
							length(Liars,N),
							Liars #:: [0,1],
							NumofLiars #=< N,
							NumofLiars #= sum(Liars),
							find_liars(List,Liars,NumofLiars),
							search(Liars,0,input_order,indomain,complete,[]).


find_liars([],[],_).
find_liars([X|List],[Y|Liars],NumofLiars) :- find_liars(List,Liars,NumofLiars), Y #= (X #> NumofLiars).


genrand(N, List) :- length(List, N), make_list(N, List).

make_list(_, []).
make_list(N, [X|List]) :- random(R), X is R mod (N+1), make_list(N, List).