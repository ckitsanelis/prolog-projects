:- set_flag(print_depth,1000).

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).
:- lib(lists).

hcvrp(NCl, NVe, Timeout, Solution, Cost, Time) :-     cputime(T1), 	  
                                                      vehicles(AllVeh),
                                                      clients(AllCl),
                                                      get_items(AllVeh,NVe,Vehicles),
                                                      get_items(AllCl,NCl,Clients),
                                                      create_lists(Clients,Coord_List,Quantity_List),
                                                      initialise_solution(NCl,NVe,InSolution),
                                                      all_distances(Coord_List,Distances_List,Coord_List),
                                                      capacity_constraint(InSolution,Vehicles,Quantity_List),
                                                      numbers_first(InSolution),
                                                      visit_constraint(1,NCl,InSolution),
                                                      non_symmetric(InSolution),
                                                      travel_constraint(InSolution,Distances_List,FinalDistances),
                                                      Cost #= sum(FinalDistance),
                                                      bb_min(search(InSolution,0,input_order,indomain,complete,[]),Cost,bb_options{timeout:Timeout}),
                                                      remove_zeroes_from_all(InSolution,Solution),
                                                      cputime(T2),
                                                      Time is T2 - T1.


%% Periorismos gia tin episkepsi kathe client apo ena mono fortigo
visit_constraint(Curr,TotalClients,_) :- Curr #= TotalClients + 1, !.
visit_constraint(Curr,TotalClients,List) :- Curr #\= 1, !, occurrences(Curr,List,1), Next is Curr + 1, visit_constraint(Next,TotalClients,List).
visit_constraint(Curr,TotalClients,List) :- flatten(List,FlatList), occurrences(Curr,FlatList,1), Next is Curr + 1, visit_constraint(Next,TotalClients,FlatList).

%% Periorismos na min upervenetai to varos pou mporei na kouvalisei kathe fortigo
capacity_constraint([],[],_).
capacity_constraint([FirstDeliveries|RestD],[FirstCapacity|RestC],Quantity) :- check_weight(FirstDeliveries,Products,Quantity), sum(Products) #=< FirstCapacity, capacity_constraint(RestD,RestC,Quantity).
check_weight([],[],_).
check_weight([H|T],[Amount|Prod],Quantity) :- check_weight(T,Prod,Quantity), element(H,Quantity,Amount).

%% Gia veltistopoiisi xronou, na min emfanizontai summetrikes luseis
non_symmetric([]).
non_symmetric([FirstDeliveries|RestD]) :- element(1,FirstDeliveries,First), compare_last(FirstDeliveries,First), non_symmetric(RestD).
compare_last([],_).
compare_last([X,Y|T],F) :- !, Y #= 0 => F #> X, compare_last([Y|T],F).
compare_last([_],_).

numbers_first([]) :- !.
numbers_first([FirstDeliveries|RestD]) :- zeroes_later(FirstDeliveries), numbers_first(RestD).
zeroes_later([]).
zeroes_later([X,Y|T]) :- !, X #= 0 => Y #= 0, zeroes_later([Y|T]).
zeroes_later([_]).

travel_constraint([],_,[]).
travel_constraint([FirstDeliveries|RestD],Distances,[Total|Tail]) :- travel_constraint(RestD,Distances,Tail), calculate_total_distance(FirstDeliveries,0,Distances,Total).
calculate_total_distance([],B,Distances,InDistance) :- get_list(Distances,B,Temp), element(1,Temp,InDistance), !.
calculate_total_distance([H|T],B,Distances,V) :- calculate_total_distance(T,H,Distances,P), get_list(Distances,H,Temp), BB is B + 1, element(BB,Temp,Val), V is P + Val.


initialise_solution(_,0,[]) :- !.
initialise_solution(NCl,NVe,[List|Tail]) :- Ext is NCl + 1, length(List,Ext), List #:: 0..NCl, NN is NVe - 1, initialise_solution(NCl,NN,Tail).

remove_zeroes_from_all([],[]).
remove_zeroes_from_all([H|T],[NH|NT]) :- remove_zeroes_from_all(T,NT), remove_zeroes_from_one(H,NH).

remove_zeroes_from_one([H|_],[]) :- H #= 0, !.
remove_zeroes_from_one([H|T],[H|R]) :- remove_zeroes_from_one(T,R).

eucl_distance(X1,Y1,X2,Y2,D) :- D1 is sqrt((X2-X1)^2 + (Y2-Y1)^2), D2 is D1 * 1000, round(D2,D).

%% Dimiourgia listas me oles tis pithanes apostaseis
all_distances([],[],_).
all_distances([(X,Y)|T],[[NewD|NewList]|Rest],Coord_List) :- all_distances(T,Rest,Coord_List), generate_distances((X,Y),Coord_List,NewList), eucl_distance(X,Y,0,0,NewD).

generate_distances(_,[],[]).
generate_distances((X,Y),[(A,B)|RestCoord],[NewD|Distances]) :- generate_distances((X,Y),RestCoord,Distances), eucl_distance(X,Y,A,B,NewD).

%% Dimiourgia listas posotitwn kai listas suntetagmenwn apo tous clients
create_lists([],[],[]).
create_lists([c(A,X,Y)|R],[(X,Y)|L1],[A|L2]) :- create_lists(R,L1,L2).

%% Sunartisi gia na pairnoume mono osa stoixeia theloume apo to vehicles kai to clients
get_items(_,0,[]) :- !.
get_items([H|T],N,[H|List]) :- N1 is N - 1, get_items(T,N1,List).

%% Sunartisi gia na epistrefei mia lista apo lista listwn
get_list([H|_],1,H) :- !.
get_list([_|T],N,R) :- NN is N - 1, get_list(T,NN,R).

vehicles([35, 40, 55, 15, 45, 25, 85, 55]).

clients([c(15,  77,  97), c(23, -28,  64), c(14,  77, -39),
         c(13,  32,  33), c(18,  32,   8), c(18, -42,  92),
         c(19,  -8,  -3), c(10,   7,  14), c(18,  82, -17),
         c(20, -48, -13), c(15,  53,  82), c(19,  39, -27),
         c(17, -48, -13), c(12,  53,  82), c(11,  39, -27),
         c(15, -48, -13), c(25,  53,  82), c(14, -39,   7),
         c(22,  17,   8), c(23, -38,  -7)]).
