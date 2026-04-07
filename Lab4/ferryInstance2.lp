#const steps=15.

car(car1;car2;car3;car4).
locations(a;b).

at(car1,a,0)
at(car2,a,0).
at(car3,a,0).
at(car4,a,0).

at(ferry,a,0).

goal(T):-
  at(car1,b,T),
  at(car2,b,T),
  at(car3,b,T),
  at(car4,b,T).
  