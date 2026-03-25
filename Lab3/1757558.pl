% Angad Chahil
% ID- 1757558
% CCID - achahil
% CMPUT 325




% Question  1 
% SO S3 is the set difference betweeen S2 and S1. 
% base case would be empty S1, maybe use a cut to not evalute S2 in taht case 
% we can like recurse through S1 and just run it with member on the rest of S2
% if true is member then we excldue, if false we append the head we took from S1 into S3 

% base case 
setDiff([],_,[]).

setDiff([H|T],S2,S3) :- 
    member(H,S2),
    setDiff(T,S2,S3).

setDiff([H|T], S2, [H|S3]) :-
    \+ member(H,S2),
    setDiff(T,S2,S3).


% Question 2, we just switch the frist two elements and then leave them and send the rest into another call 


% base case


exchange([],[]).
% a list with only one elment itll be the basecase if list is odd

exchange([A],[A]).

exchange([A,B|T], [B,A|R]) :-
    exchange(T,R).


% Question 3 , so we add into L1 our output. L is nested so need to recurse into the lists


filter([], _, _, []).

filter([H|T], OP, N, L1) :-
    is_list(H),         % so yes its a list then we recurse into the list, fails jump to next clause which handles it beinga  number 
    filter(H, OP, N, Lh),
    filter(T, OP, N, Lt),       % do for both the head that we popped out adn the tail, 
    append(Lh, Lt, L1).         % combines those two outputs int one list 

filter([H|T], OP, N, [H|L1]) :-
    satisfies(OP, H, N),
    filter(T, OP, N, L1).       % run filter on the rest

filter([H|T], OP, N, L1) :-     % fails to satfify the condiion
    \+ satisfies(OP, H, N),
    filter(T, OP, N, L1).           % skip the head recurse on the tail 

satisfies(equal, X, N) :-
    X =:= N.
satisfies(greaterThan, X, N) :-
    X > N.

satisfies(lessThan, X, N) :-
    X < N.



% Queston 4 , we need to count every occurence of every atom and just return biggest oe 


countOccurence(_, [], 0).

countOccurence(X, [X|T], N) :-
    countOccurence(X, T, N1),        % if the head is equal  x then we add 1 and recurse the rest 
    N is N1 + 1.

countOccurence(X, [H|T], N) :-
    X \= H, 
    countOccurence(X, T, N).       % head diff just recuese the tail 

countMax([H|T], N) :-               % traet atom 1 as the current max til a beter is found 
    countOccurence(H, [H|T], C),
    countMaxHelper(T, [H|T], H, C, N). 



 % base case we have gone throiuhg all atoms 

countMaxHelper([], _, BestAtom, BestCount, [BestAtom, BestCount]).


countMaxHelper([H|T], Original, BestAtom, BestCount, Result) :-     % the count is bigger than our curernt so we assing it as the biggest curerntyl 
    countOccurence(H, Original, Count ),
    Count > BestCount,
    countMaxHelper(T, Original, H, Count, Result).

countMaxHelper([H|T], Original, BestAtom, BestCount, Result) :-        % not bigger keep current winner 
    countOccurence(H, Original, Count), 
    Count =< BestCount,
    countMaxHelper(T, Original, BestAtom, BestCount, Result). 


% Question 5, replace the elemtn if its in the S list, and its not there then we leave 
% L may be nested so need to recurse intio 

sub([], _, []).

sub([H|T], S, [H1|T1]) :-
    is_list(H),
    sub(H, S, H1),          % if its lsit recurse into the H and the T gets it on call 
    sub(T, S, T1).

sub([H|T], S, [R|T1]) :-
    replaceAtom(H,S,R),
    sub(T,S,T1).        % recurse on just the tail since head is atom 

replaceAtom(X, [], X). 

replaceAtom(X, [[X,E]|_], E).       % first pair is a match so we can do the replacement

replaceAtom(X, [[Y,_]|T], R) :-         % first paiur did not match 
    X \= Y,
    replaceAtom(X, T, R). 




% Question 6
% needs to generate the subsets, and kee the ones that have pairs where the nodes are connected 

clique(L) :-
    findall(X, node(X), Nodes),
    subset(Nodes, L),
    isClique(L). 

% each atom we can either inlcude or exlude so we recurse on both of those otpions 
subset([],[]).
subset([H|T], [H|Rest]) :-          % one subset can start with the front 
    subset(T, Rest).

subset([_|T], Rest) :-          % the subset can contain anything other than the head 
    subset(T, Rest).


% check if the subset we have is a clique 
isClique([]).
          
isClique([H|T]) :-
    connectedTo(H,T),
    isClique(T).


% checks if one nodes is conncted to eveyr other node thats in the list 
connectedTo(_, []).
connectedTo(X, [H|T]) :-
    connected(X, H),            % ture if X conncted to H
    connectedTo(X, T).          % true if connected to the rest, we just recurse on the tail 

% edges undirected checks as longas one direction is true we are good 
connected(X, Y) :-
    edge(X, Y).
connected(X, Y) :-
    edge(Y, X).


% Quesito 7 , 

:- use_module(library(lists)).

convert(Term, Result) :-
    outsideQuotes(Term, Result).

outsideQuotes([],[]).


% rempves the spaces annd we remove char to w before we get to q 
outsideQuotes([e|T], R) :-
    outsideQuotes(T,R). 

% if we have a q then we recurse for tbe nbext q, if not found we have to replace har with w 
outsideQuotes([q|T], [q|R]) :-
    append(Between, [q|Rest], T), !,
    append(Between, [q|R1], R),     % found q, copy string between, recurse on the rest 
    outsideQuotes(Rest,R1).

% this one if we dont have found a second q 
outsideQuotes([q|T], [q|R]) :-
    outsideQuotes(T, R).

% outside the quotes 
outsideQuotes([_|T], [w|R]) :-
    outsideQuotes(T, R).

