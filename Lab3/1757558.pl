# Question  1 
# SO S3 is the set difference betweeen S2 and S1. 
# base case would be empty S1, maybe use a cut to not evalute S2 in taht case 
# we can like recurse through S1 and just run it with member on the rest of S2
# if true is member then we excldue, if false we append the head we took from S1 into S3 

# base case 
setDiff([],_,[]).

setDiff([H|T],S2,S3) :- 
    member(H,S2),
    setDiff(T,S2,S3).

setDiff([H|T], S2, [H|S3]) :-
    \+ member(H,S2),
    setDiff(T,S2,S3).