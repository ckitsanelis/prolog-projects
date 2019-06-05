
decode_rl([],[]) :- !.
decode_rl([(X,1)|Tail], [X|NewList]) :-  decode_rl(Tail, NewList), !.
decode_rl([(X,N)|Tail], [X|NewList]) :-  K is N-1, decode_rl([(X,K)|Tail], NewList), !.
decode_rl([X|Tail], [X|NewList]) :- decode_rl(Tail, NewList).


encode_rl([],[]) :- !.
encode_rl([X|Tail],[X|NewList]) :- multiple(X,Tail,1,N,Remaining), N=:=1, encode_rl(Remaining,NewList), !.
encode_rl([X|Tail],[(X,N)|NewList]) :- multiple(X,Tail,1,N,Remaining), encode_rl(Remaining,NewList).

%% Sunartisi pou upologizei poses fores uparxei ena sumbolo sti lista
%% To C tha epistrepsei to plithos enos sugekrimenou sumbolou kai to Remaining einai to upoloipo tis arxikis listas pou den exei diavastei
multiple(X,[],C,C,[]) :- !.
multiple(X,[Z|Old],C,C,[Z|Old]) :- X \= Z, !.
multiple(X,[X|Old],C,K,Remaining) :- Α is C+1, multiple(X,Old,Α,K,Remaining).


%% ?- decode_rl([(a,3),(b,2),c,(d,4),e], L).
%% ?- decode_rl([(f(5,a),7)], L).
%% ?- decode_rl([g(X),(h(Y),3),k(Z),(m(W),4),n(U)], L).

%% ?- encode_rl([a,a,a,b,b,c,d,d,d,d,e], L).
%% ?- encode_rl([f(5,a),f(5,a),f(5,a),f(5,a),f(5,a),f(5,a),f(5,a)], L).
%% ?- encode_rl([g(X),h(Y),h(Y),h(Y),k(Z),m(W),m(W),m(W),m(W),n(U)], L).


%% ?- encode_rl([p(3),p(X),q(X),q(Y),q(4)], L).

/**

To apotelesma tis teleutaias entolis einai to exis:
X = 3
Y = 3
L = [(p(3), 2), (q(3), 2), q(4)]

Sto p(X) to X einai metavliti opote otan to sugrinei me to p(3) to thewrei idio an thesei X=3.
Sti sunexeia to q(X) to thewrei idio me q(3) kai omoiws me prin sugrinwntas to me to q(Y), einai to idio sumbolo an Y=X=3. Etsi exoume 2 periptwseis p(3) kai 2 periptwseis q(3).
Profanws to q(4) einai diaforetiko sumbolo apo to q(3) opote uparxei mono mia periptwsi tou.
*/


