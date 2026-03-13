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
