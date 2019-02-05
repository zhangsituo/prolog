% tp noté

%1.
appartient(_,[]).
appartient(X,[Y|T]) :- 
    X \= Y,
    appartient(X,T).
appartient(X,[X|_]).

%2.
ajout_element(X,T,[X|T]).

%3.
inclus(L1,L2):-
    append(_,BL2,L2),
    append(L1,_,BL2).

%4.
union([],Y,Y).
union([H|T],Y,L):-
    union(T,Y,LL),
    L = [H|LL].

%5.
intersection(X,Y,Z):-
    member(A,X),
    member(A,Y),
    member(A,Z).

%6.
enlever_doublons([],[]).
enlever_doublons([T|R],[T|R2]):-
    supprimer(T,R,R1),enlever_doublons(R1,R2).
%6.
enlever_doublons(X,Y):-
    reverse(X,RX),
    remove_dups(RX,RY),
    reverse(RY,Y).

remove_dups([], []).

remove_dups([First | Rest], NewRest) :- 
    member(First, Rest), 
    remove_dups(Rest, NewRest).

remove_dups([First | Rest], [First | NewRest]) :- 
    not(member(First, Rest)), 
    remove_dups(Rest, NewRest).

%7. 
trier(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).

insert(X,[Y|T],[Y|NT]):-X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-X=<Y.
insert(X,[],[X]).

%8. 
ensemble_sous_ensembles(X,Y):-
    findall(L,sublist(X,L),HY),
    Y=[[]|HY].

sublist(L, S) :-
    length(L, N),
    between(1, N, M),
    length(S, M),
    append([_,S,_], L).

%9.
ensemble_permutations(X,Y):-
    findall(L,perm(X,L),Y).

perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).  
perm([],[]).

takeout(X,[X|R],R).  
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).
