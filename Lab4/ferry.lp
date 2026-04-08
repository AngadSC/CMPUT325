% Initially, there are cars at various locations, and there is a
% ferry at some location. The ferry can only transport one car at
% a time and the goal is to transport all cars to their
% destinations. No parallel actions are allowed.

#show board/3.
#show move/4.
#show unboard/3.


time(0..steps).

% Clingo processes "safe programs": any variable occurring in a
% negative literal of rule r must appear in a positive atom in the body of r.
%
% There is nothing wrong to use domains predicates. One may first write
% these predicates and then comment the unnecessary ones out (as shown).
% Nothing wrong to leave them there.
% Not all of them can be removed, specially when there is a possibility
% a variable can be instantiated (during grounding) to something unintended.
% You may discover this during debugging using the "#show" derivative. 


% actions
{board(Car,Loc,T)} :-
        car(Car),
       location(Loc), time(T),
        empty(ferry,T),
        at(Car,Loc,T),
        at(ferry,Loc,T),
        not moving(ferry,Loc,T),
        not goal(T).


{unboard(Car,Loc,T)} :-
    car(Car),
    location(Loc), time(T),
    in(ferry,Car, T),
    at(ferry,Loc,T),
    not moving(ferry,Loc,T),
    not goal(T).
     
{move(ferry,From,To,T)} :-
        location(From), location(To), time(T),,
        at(ferry,From,T),
        From != To,
        not goal(T).

moving(ferry,From,T):-     % irrelevant of where ferry moves to
       location(From),location(Loc), time(T),
       at(ferry,From,T),
       move(ferry,From,Loc,T).

empty(ferry,T):- time(T), not occupied(ferry,T).
occupied(ferry,T) :- in(ferry,_,T).


%fluents
in(ferry,Car,T+1):-   %an action causes a property to hold
%       car(Car), location(Loc), time(T),
        at(ferry,Loc,T),
        board(Car,Loc,T).

in(ferry,Car,T+1):-
%       car(Car),
        time(T),
        in(ferry,Car,T),
        not affected0(Car,T).

affected0(Car,T) :-
%       time(T), car(Car), location(Loc),
        unboard(Car,Loc,T).

% Cannot replace the above definition of in/3 by below, which says that
% Car is in ferry at T+1 if at T there is a location Loc
% s.t. Car is not unboarded - not intended!
%
% in(ferry,Car,T+1):-
%        car(Car), time(T),
%        in(ferry,Car,T),
%        not unboard(Car,Loc,T).

at(ferry,Loc,T+1):-
%       car(Car), location(Loc), time(T),
        at(ferry,Loc,T),
        board(Car,Loc,T).

at(ferry,Loc,T+1):-
%       car(Car), location(Loc), time(T),
        at(ferry,Loc,T),
        unboard(Car,Loc,T).

at(ferry,Loc,T+1):-
%       location(Loc),
        time(T),
        at(ferry,Loc,T), %if we don't have this line, what could happen?
                         %A: ferry can be everywhere
        not moving(ferry,Loc,T).

at(ferry,To,T+1):-
%       location(To),location(From),
        time(T),
        at(ferry,From,T),
        move(ferry,From,To,T).

at(Car,Loc,T+1):-
        car(Car),  % not commented out - don't want Car instantiated to ferry
%       location(Loc), time(T),
        unboard(Car,Loc,T).

at(Car,Loc,T+1):-    %frame axiom
        car(Car), location(Loc), time(T),
        at(Car,Loc,T),
        not board(Car,Loc,T).

goal(T+1):-  time(T), goal(T).
     %once goal is achieved, goal(T) is true for all T > k.

goal :- time(T), goal(T).
:- not goal.


% First, define unboard(Car,Loc,T). Then, run the code with the input files, ferryIn0.lp, and ferryIn1.lp, and ferryIn2.lp,
% respectively. You will find that the code does not really work, due to missing some constraints. For example, 
% you need to say that ferry cannot be moved to two different locations at the same time, and possibly others.  

% constraints
:- board(C1,L1,T), board(C2,L2,T), C1 !=C2.
:- unboard(C1,L1,T), unboard(C2,L2,T), C1 != C2.
:- move(ferry, From, To1,T), move(ferry,From,To2,T), To1 !=To2.
:- board(_,_,T), unboard(_,_,T).
:- board(_,_,T), move(ferry,_,_,T).
:- unboard(_,_,T), move(ferry, _,_,T).
:- in(ferry,C1,T), in(ferry,C2,T), C1 !=C2.
:- at(ferry,L1,T), at(ferry,L2,T), L1 !=L2.
:- at(C,L,T), in(ferry,C,T).
