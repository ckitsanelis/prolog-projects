:- set_flag(print_depth, 1000). 

:- lib(ic).
:- lib(branch_and_bound).



tents(RowTents, ColumnTents, Trees, Tents) :- 	length(RowTents,Rows),
												length(ColumnTents,Columns),
												L is Rows * Columns,
												length(InitialTents,L),
												InitialTents #:: [0,1],
												tree_position_constraint(Trees,InitialTents,Columns),
												row_constraint(RowTents,InitialTents,Columns),
												column_constraint(ColumnTents,InitialTents,Columns,Rows),
												tree_tent_constraint(Trees,InitialTents,Columns,Rows),
												alone_tent(InitialTents,InitialTents,1,1,0,Columns,Rows),
												Cost #= sum(InitialTents),
												bb_min(search(InitialTents,0,input_order,indomain,complete,[]),Cost, bb_options{strategy:continue}),

												length(FullTents,L),
												FullTents #:: [0,1],
												tree_position_constraint(Trees,FullTents,Columns),
												row_constraint(RowTents,FullTents,Columns),
												column_constraint(ColumnTents,FullTents,Columns,Rows),
												tree_tent_constraint(Trees,FullTents,Columns,Rows),
												alone_tent(FullTents,FullTents,1,1,0,Columns,Rows),
												Cost #= sum(FullTents),
												search(FullTents,0,input_order,indomain,complete,[]),
												get_results(FullTents,Columns,1,Rows,1,Tents).


%% Periorismos na uparxei toulaxiston ena tent dipla se dentro
tree_tent_constraint([],_,_,_).
tree_tent_constraint([X-Y|Trees],Tents,Columns,Rows) :- K is (X - 1) * Columns + (Y - 1), neighbors(X,Y,Columns,Rows,K,List),
														get_elements(List,Tents,NewList), sum(NewList) #> 0, tree_tent_constraint(Trees,Tents,Columns,Rows).

%% Periorismos na min uparxoun geitonika tents
alone_tent([],_,_,_,_,_,_).
alone_tent([Z|Rest],AllTents,X,Y,K,Columns,Rows) :- Y #= Columns, !, neighbors(X,Y,Columns,Rows,K,List), get_elements(List,AllTents,NewList), Z #= 1 => sum(NewList) #= 0,
													X1 is X + 1, Y1 is 1, K1 is K + 1, alone_tent(Rest,AllTents,X1,Y1,K1,Columns,Rows).
alone_tent([Z|Rest],AllTents,X,Y,K,Columns,Rows) :- neighbors(X,Y,Columns,Rows,K,List), get_elements(List,AllTents,NewList), Z #= 1 => sum(NewList) #= 0,
													Y1 is Y + 1, K1 is K + 1, alone_tent(Rest,AllTents,X,Y1,K1,Columns,Rows).

%% Pairnei tis times pou prepei apo tin duadiki lista
get_elements([],_,[]).
get_elements([X|Rest], [Z|Tents],[Z|List]) :- X #= 0, get_elements(Rest,[Z|Tents],List).
get_elements([X|Rest], [_|Tents],List) :- X #> 0, X1 is X - 1, get_elements([X1|Rest],Tents,List).

%% Apofasizei poies einai oi geitonikes theseis mias timis kai vazei tis apostaseis tous se mia lista
neighbors(X,Y,Columns,_,K,[N1,N2,N3]) :- X #= 1, Y #= 1, !, N1 is K + 1, N2 is Columns - 1, N3 is 1.
neighbors(X,Y,Columns,_,K,[N1,N2,N3]) :- X #= 1, Y #= Columns, !, N1 is K - 1, N2 is Columns, N3 is 1.
neighbors(X,_,Columns,_,K,[N1,N2,N3,N4,N5]) :- X #= 1, !, N1 is K - 1, N2 is 2, N3 is Columns - 2, N4 is 1, N5 is 1.
neighbors(X,Y,Columns,Rows,K,[N1,N2,N3]) :- X #= Rows, Y #= 1, !, N1 is K - Columns, N2 is 1, N3 is Columns.
neighbors(X,Y,Columns,Rows,K,[N1,N2,N3]) :- X #= Rows, Y #= Columns, !, N1 is K - Columns - 1, N2 is 1, N3 is Columns - 1.
neighbors(X,_,Columns,Rows,K,[N1,N2,N3,N4,N5]) :- X #= Rows, !, N1 is K - Columns - 1, N2 is 1, N3 is 1, N4 is Columns - 2, N5 is 2.
neighbors(_,Y,Columns,_,K,[N1,N2,N3,N4,N5]) :- Y #= 1, !, N1 is K - Columns, N2 is 1, N3 is Columns, N4 is Columns - 1, N5 is 1.
neighbors(_,Y,Columns,_,K,[N1,N2,N3,N4,N5]) :- Y #= Columns, !, N1 is K - Columns - 1, N2 is 1, N3 is Columns - 1, N4 is Columns, N5 is 1.
neighbors(_,_,Columns,_,K,[N1,N2,N3,N4,N5,N6,N7,N8]) :- N1 is K - Columns - 1, N2 is 1, N3 is 1, N4 is Columns - 2, N5 is 2, N6 is Columns - 2, N7 is 1, N8 is 1.


%% Periorismos na min uparxei tent panw se dentro
tree_position_constraint([],_,_).
tree_position_constraint([X-Y|Trees],Tents,Columns) :- K is (X - 1) * Columns + (Y - 1), no_tent(Tents,K), tree_position_constraint(Trees,Tents,Columns).

no_tent([X|_],0) :- X #= 0.
no_tent([_|Tents],N) :- N #> 0, N1 is N - 1, no_tent(Tents,N1).


%% Periorismos tou plithous twn tents se kathe seira
row_constraint([],_,_).
row_constraint([X|RowTents],Tents,Columns) :- X #>= 0, !, row_elements(Tents,Columns,List,RestTents), sum(List) #=< X, !, row_constraint(RowTents,RestTents,Columns).
row_constraint([_|RowTents],Tents,Columns) :- row_elements(Tents,Columns,List,RestTents), row_constraint(RowTents,RestTents,Columns).

row_elements(Rest,0,[],Rest).
row_elements([X|Tents],N,[X|List],Rest) :- N #> 0, N1 is N - 1, row_elements(Tents,N1,List,Rest).


%% Periorismos tou plithous twn tents se kathe stili
column_constraint([],_,_,_).
column_constraint([X|ColumnTents],[Z|Tents],Columns,Rows) :- X #>= 0, column_elements([Z|Tents],1,Columns,Rows,List), sum(List) #=< X, !, column_constraint(ColumnTents,Tents,Columns,Rows).
column_constraint([X|ColumnTents],[_|Tents],Columns,Rows) :- X #< 0, column_constraint(ColumnTents,Tents,Columns,Rows).

column_elements(_,_,_,0,[]).
column_elements([X|Tents],C,Columns,RemRows,[X|List]) :- C #= 1, R1 is RemRows - 1, column_elements(Tents,Columns,Columns,R1,List).
column_elements([_|Tents],C,Columns,RemRows,List) :- C #> 1, C1 is C - 1, column_elements(Tents,C1,Columns,RemRows,List).

%% Metatrepei ti duadiki lista sti morfi pou theloume na emfanizetai
get_results([],_,_,_,_,[]).
get_results([1|Tents],Columns,CurrColumns,Rows,CurrRows,[CurrRows-CurrColumns|FinalList]) :- CurrColumns #= Columns, !, R1 is CurrRows + 1, C1 is 1, get_results(Tents,Columns,C1,Rows,R1,FinalList).
get_results([1|Tents],Columns,CurrColumns,Rows,CurrRows,[CurrRows-CurrColumns|FinalList]) :- C1 is CurrColumns + 1, get_results(Tents,Columns,C1,Rows,CurrRows,FinalList).
get_results([0|Tents],Columns,CurrColumns,Rows,CurrRows,FinalList) :- CurrColumns #= Columns, !, R1 is CurrRows + 1, C1 is 1, get_results(Tents,Columns,C1,Rows,R1,FinalList).
get_results([0|Tents],Columns,CurrColumns,Rows,CurrRows,FinalList) :- C1 is CurrColumns + 1, get_results(Tents,Columns,C1,Rows,CurrRows,FinalList).