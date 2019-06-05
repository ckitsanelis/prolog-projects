:- set_flag(print_depth, 1000). 

dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
               (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
                     (2,2),(2,3),(2,4),(2,5),(2,6),
                           (3,3),(3,4),(3,5),(3,6),
                                 (4,4),(4,5),(4,6),
                                       (5,5),(5,6),
                                             (6,6)]).

frame([[3,1,2,6,6,1,2,2],
       [3,4,1,5,3,0,3,6],
       [5,6,6,1,2,4,5,0],
       [5,6,4,1,3,3,0,0],
       [6,1,0,6,3,2,4,0],
       [4,1,5,2,4,3,5,5],
       [4,1,0,2,4,5,2,0]]).


put_dominos :-  dominos(Dominos), frame(Frame), frame([_,SecondList|_]),
				        count_rows(R,Frame), count_columns(C,Frame), R1 is (R*2)-1, C1 is (C*2)-1,
				        find_positions(Dominos,R,C,Frame,SecondList,FullList),
                my_sort(FullList,SortedList),
                place_tiles(SortedList,Tiles),
                make_empty_list(R1,C1,C1,[],EmptyList),
                result_list(Tiles,EmptyList,PosList),
                print_result(PosList).

%% Vriskei oles tis pithanes theseis gia kathe domino sto frame
find_positions([],_,_,_,_,[]).
find_positions([(A,B)|Dominos],R,C,Frame,SecondList,[((A,B),LS,DomList)|FinalList]) :-    horizontal_positions((A,B),C,1,2,1,Frame,HorList),
                                                                                          vertical_positions((A,B),R,C,1,2,1,SecondList,Frame,VerList),
                                                                                          merge_lists(HorList,VerList,DomList), !, num_of_positions(LS,DomList),
                                                                                          find_positions(Dominos,R,C,Frame,SecondList,FinalList).

place_tiles([],[]).
place_tiles([(D,_,[P1,P2|RestPositions])|Tail],[[D,P1,P2]|FinalList]) :- update_all_lists(P1,P2,Tail,NewList), my_sort(NewList,SortedList), place_tiles(SortedList,FinalList).
place_tiles([(D,L,[_,_|RestPositions])|Tail],FinalList) :- place_tiles([(D,L,RestPositions)|Tail],FinalList).

%% Orismata: Current domino, Plithos stilwn, Thesi X1, Thesi X2, Thesi Y, Remaining frame, List of positions
horizontal_positions(_,_,_,_,_,[],[]).
horizontal_positions((A,B),C,D1X,D2X,DY,[_|Rest],PosList) :- D1X==C, DDY is DY + 1, horizontal_positions((A,B),C,1,2,DDY,Rest,PosList), !.
horizontal_positions((A,B),C,D1X,D2X,DY,[[X,Y|RemFrame]|Rest],[(DY,D1X),(DY,D2X)|PosList]) :- A==X, B==Y, DD1X is D1X+2, horizontal_positions((A,B),C,D2X,DD1X,DY,[[Y|RemFrame]|Rest],PosList), !.
horizontal_positions((A,B),C,D1X,D2X,DY,[[X,Y|RemFrame]|Rest],[(DY,D2X),(DY,D1X)|PosList]) :- A==Y, B==X, DD1X is D1X+2, horizontal_positions((A,B),C,D2X,DD1X,DY,[[Y|RemFrame]|Rest],PosList), !.
horizontal_positions((A,B),C,D1X,D2X,DY,[[X,Y|RemFrame]|Rest],PosList) :- DD1X is D1X+2, horizontal_positions((A,B),C,D2X,DD1X,DY,[[Y|RemFrame]|Rest],PosList).

vertical_positions(_,R,_,_,D2Y,_,SecondList,[[],[]|_],[]) :- R==D2Y, !.
vertical_positions((A,B),R,C,D1Y,D2Y,DX,SecondList,[_,_,Third|Rest],PosList) :- DX>C, DDY is D1Y+2, !, vertical_positions((A,B),R,C,D2Y,DDY,1,Third,[SecondList,Third|Rest],PosList).
vertical_positions((A,B),R,C,D1Y,D2Y,DX,SecondList,[[X|List1],[Z|List2]|Rest],[(D1Y,DX),(D2Y,DX)|PosList]) :- A==X, B==Z, DDX is DX+1, !, vertical_positions((A,B),R,C,D1Y,D2Y,DDX,SecondList,[List1,List2|Rest],PosList).
vertical_positions((A,B),R,C,D1Y,D2Y,DX,SecondList,[[X|List1],[Z|List2]|Rest],[(D2Y,DX),(D1Y,DX)|PosList]) :- A==Z, B==X, DDX is DX+1, !, vertical_positions((A,B),R,C,D1Y,D2Y,DDX,SecondList,[List1,List2|Rest],PosList).
vertical_positions((A,B),R,C,D1Y,D2Y,DX,SecondList,[[X|List1],[Z|List2]|Rest],PosList) :- DDX is DX+1, !,vertical_positions((A,B),R,C,D1Y,D2Y,DDX,SecondList,[List1,List2|Rest],PosList).

%% Diagrafei apo tis listes thesewn twn domino, tis theseis pou exoun idi piastei
update_all_lists(_,_,[],[]).
update_all_lists((A,B),(C,D),[(T,_,List)|Rest],[(T,Len,UpdatedList)|FinalList]) :- update_one_list((A,B),(C,D),List,UpdatedList), num_of_positions(Len,UpdatedList), update_all_lists((A,B),(C,D),Rest,FinalList).

update_one_list(_,_,[],[]).
update_one_list((A,B),(C,D),[(Q,W),(E,R)|Rest],NewList) :- ((A==Q),(B==W);(A==E),(B==R)), !, update_one_list((A,B),(C,D),Rest,NewList).
update_one_list((A,B),(C,D),[(Q,W),(E,R)|Rest],NewList) :- ((C==Q),(D==W);(C==E),(D==R)), !, update_one_list((A,B),(C,D),Rest,NewList).
update_one_list((A,B),(C,D),[(Q,W),(E,R)|Rest],[(Q,W),(E,R)|NewList]) :- update_one_list((A,B),(C,D),Rest,NewList).

%% Quicksort gia tin taxinomisi twn domino se auxousa seira gia na epilexoume to epomeno domino pou tha topothetisoume
my_sort(List,Sorted) :- quick_sort(List,[],Sorted).
quick_sort([],Acc,Acc).
quick_sort([(A,B,C)|Tail],Acc,Sorted) :- pivot((A,B,C),Tail,L1,L2), !,quick_sort(L1,Acc,Sorted1), quick_sort(L2,[(A,B,C)|Sorted1],Sorted).

pivot(H,[],[],[]).
pivot((A1,B1,C1),[(A2,B2,C2)|Tail],[(A2,B2,C2)|List1],List2) :- B2>B1, pivot((A1,B1,C1),Tail,List1,List2).
pivot((A1,B1,C1),[(A2,B2,C2)|Tail],List1,[(A2,B2,C2)|List2]) :- B2=<B1, pivot((A1,B1,C1),Tail,List1,List2).

%% Dimiourgia listas sto megethos pou theloume kai gemati me -5(isodunamei me keno stin ektupwsi)
make_empty_list(0,0,_,Acc,Acc).
make_empty_list(R,C,InitialC,[P|Acc],Empty) :- C>0, C1 is C-1, K is -5, make_empty_list(R,C1,InitialC,[[K|P]|Acc],Empty), !.
make_empty_list(R,C,InitialC,Acc,Empty) :- R1 is R-1, make_empty_list(R1,InitialC,InitialC,[[]|Acc],Empty).

%% Diavazei ti lista me ta domino kai tis theseis tous kai ta grafei sti lista pou xrisimopoioume stin ektupwsi
result_list([],Acc,Acc).
result_list([[(A,B),(Y,X1),(Y,X2)]|Rest],InitalList,NewList) :- X1<X2, !, NX is (X1*2)-1, NY is (Y*2)-1, replace_horizontal_items(InitalList,A,B,NY,NX,TempList), result_list(Rest,TempList,NewList).
result_list([[(A,B),(Y,X1),(Y,X2)]|Rest],InitalList,NewList) :- X1>X2, !, NX is (X2*2)-1, NY is (Y*2)-1, replace_horizontal_items(InitalList,B,A,NY,NX,TempList), result_list(Rest,TempList,NewList).
result_list([[(A,B),(Y1,X),(Y2,X)]|Rest],InitalList,NewList) :- Y1<Y2, !, NX is (X*2)-1, NY1 is (Y1*2)-1, replace_item(InitalList,A,NY1,NX,TempList1), NY2 is (Y2*2)-1, replace_item(TempList1,B,NY2,NX,TempList2),
                                                                NY is NY1+1, replace_item(TempList2,-3,NY,NX,TempList3), result_list(Rest,TempList3,NewList).
result_list([[(A,B),(Y1,X),(Y2,X)]|Rest],InitalList,NewList) :- Y1>Y2, !, NX is (X*2)-1, NY1 is (Y1*2)-1, replace_item(InitalList,A,NY1,NX,TempList1), NY2 is (Y2*2)-1, replace_item(TempList1,B,NY2,NX,TempList2),
                                                                NY is NY2+1, replace_item(TempList2,-3,NY,NX,TempList3), result_list(Rest,TempList3,NewList).


replace_item([H|Rest],A,Y,X,[H|NewList]) :- Y>1, Y1 is Y-1, !, replace_item(Rest,A,Y1,X,NewList).
replace_item([[H|T]|Rest],A,Y,X,[[H|Q]|NewList]) :- X>1, X1 is X-1, !, replace_item([T|Rest],A,Y,X1,[Q|NewList]).
replace_item([[H|T]|Rest],A,Y,X,[[A|T]|Rest]).


replace_horizontal_items([H|Rest],A,B,Y,X,[H|NewList]) :- Y>1, Y1 is Y-1, !, replace_horizontal_items(Rest,A,B,Y1,X,NewList).
replace_horizontal_items([[H|T]|Rest],A,B,Y,X,[[H|Q]|NewList]) :- X>1, X1 is X-1, !, replace_horizontal_items([T|Rest],A,B,Y,X1,[Q|NewList]).
replace_horizontal_items([[H,H1,H2|T]|Rest],A,B,Y,X,[[A,-2,B|T]|Rest]).

%% Diavazei ti lista pou dimiourgisame kai tin ektupwnei opws theloume
print_result([]).
print_result([[]|Rest]) :- write("\n"), !, print_result(Rest).
print_result([[H|T]|Rest]) :- H==(-3), write("|"), !, print_result([T|Rest]).
print_result([[H|T]|Rest]) :- H==(-2), write("-"), !, print_result([T|Rest]).
print_result([[H|T]|Rest]) :- H==(-5), write(" "), !, print_result([T|Rest]).
print_result([[H|T]|Rest]) :- write(H), !, print_result([T|Rest]).

count_rows(0,[]).
count_rows(N,[[X|Tail]|Rest]) :- count_rows(N1,Rest), N is N1 + 1.

count_columns(0,[]).
count_columns(N,[[X|Tail]|Rest]) :- count_columns(N,[X|Tail]), !.
count_columns(N,[X|Tail]) :- count_columns(N1,Tail), N is N1 + 1.

merge_lists([],[],[]).
merge_lists([],[(A1,A2),(A3,A4)|List2],[(A1,A2),(A3,A4)|List]) :- merge_lists([],List2,List).
merge_lists([(A1,A2),(A3,A4)|List1],[],[(A1,A2),(A3,A4)|List]) :- merge_lists(List1,[],List).
merge_lists([(A1,A2),(A3,A4)|List1],[(B1,B2),(B3,B4)|List2],[(A1,A2),(A3,A4),(B1,B2),(B3,B4)|List]) :- merge_lists(List1,List2,List).

num_of_positions(0,[]).
num_of_positions(N,[_,_|T]) :- num_of_positions(N1,T), N is N1+1.