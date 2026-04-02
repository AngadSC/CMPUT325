% Angad Chahil
% ID- 1757558
% CCID - achahil
% CMPUT 325

% Question 1 Part A
% We have all the marks we need to concert into their weights and add together 
:- dynamic setup/4.

query1(Semester, Name, Total) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final), % gets the value from db
    component_score(Semester, as1, As1, S1),
    component_score(Semester, as2, As2, S2),
    component_score(Semester, as3, As3, S3),
    component_score(Semester, as4, As4, S4),
    component_score(Semester, midterm, Midterm, S5),
    component_score(Semester, final, Final, S6),
    Total is S1 + S2 + S3 + S4 + S5 + S6.

component_score(Semester, Type, Mark, WeightedScore) :-
    setup(Semester,Type, Max, Percentage),
    WeightedScore is (Mark / Max) * Percentage * 100.


% Question 1 Part B
% students with a final mark greater than midterm mark 

query2(Semester, L) :-
    findall(Name, improved_final(Semester, Name), L).

improved_final(Semester, Name) :-
    c325(Semester, Name, _, _, _, _, Midterm, Final),
    setup(Semester, midterm, MidMax, _),
    setup(Semester, final, FinalMax, _),
    MidtermPercent is Midterm / MidMax,
    FinalPercent is Final / FinalMax,
    FinalPercent > MidtermPercent. 

:- dynamic c325/8.

% Question 1 Part C
% update one mark 

query3(Semester, Name, Type, NewMark) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final), !,
    retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
    updated_record(Type, NewMark, As1, As2, As3, As4, Midterm, Final,NewAs1, NewAs2, NewAs3, NewAs4, NewMidterm, NewFinal ),
    assertz(c325(Semester, Name, NewAs1, NewAs2, NewAs3, NewAs4, NewMidterm, NewFinal)).

query3(_, _, _, _) :-
    write('record not found'), nl.

updated_record(as1, NewMark, _, As2, As3, As4, Midterm, Final, NewMark, As2, As3, As4, Midterm, Final).
updated_record(as2, NewMark, As1, _, As3, As4, Midterm, Final, As1, NewMark, As3, As4, Midterm, Final).
updated_record(as3, NewMark, As1, As2, _, As4, Midterm, Final, As1, As2, NewMark, As4, Midterm, Final).
updated_record(as4, NewMark, As1, As2, As3, _, Midterm, Final, As1, As2, As3, NewMark, Midterm, Final).
updated_record(midterm, NewMark, As1, As2, As3, As4, _, Final, As1, As2, As3, As4, NewMark, Final).
updated_record(final, NewMark, As1, As2, As3, As4, Midterm, _, As1, As2, As3, As4, Midterm, NewMark).

    
 % Question 2 
 
 :- use_module(library(clpfd)).

 encrypt(W1, W2, W3) :-
    length(W1, N),
    length(W2, N),
    length(W3, N3),
    (N3 #= N ; N3 #= N +1), % W3 can have len = W1 or one more 
% add all the lists together 
    append(W1, W2, Temp),
    append(Temp, W3, All),

    list_to_set(All, Letters),

    W1 = [Lead1|_],
    W2 = [Lead2|_],
    W3 = [Lead3|_],

% assign unique number to each letter
    Letters ins 0..9,

    all_distinct(Letters),
    Lead1 #\= 0,
    Lead2 #\= 0,
    Lead3 #\= 0,

    word_value(W1, V1),
    word_value(W2, V2),
    word_value(W3, V3),

    V1 + V2 #= V3,
    labeling([], Letters).

word_value(Word, Value) :-
    length(Word, N),
    word_value(Word, N, Value).

word_value([], _, 0).

word_value([D|Ds], N, Value) :-
    N1 #= N - 1,
    word_value(Ds, N1, Rest),
    pow10(N1, P),
    Value #= D * P + Rest. 

pow10(0,1).

pow10(N, P) :-
    N #> 0,
    N1 #= N -1,
    pow10(N1, P1),
    P #= 10 * P1.



% QUestion 3 

sudoku(Rows) :-
    grid(9, Rows),
        % Rows now is a 9x9 grid of variables
    append(Rows, Vs),
        % Vs is a list of all 9*9 variables in Rows
    Vs ins 1..9,
    xall-distinct(Rows),
        % Variables of each row get distinct values
    xtranspose(Rows, Columns),
        % get the columns of 9x9 grid
    xall-distinct(Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        % need references to rows
    blocks(As, Bs, Cs),
        % deal with three rows at a time
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

problem(P) :-
    P = [[1,_,_,8,_,4,_,_,_],
	 [_,2,_,_,_,_,4,5,6],
	 [_,_,3,2,_,5,_,_,_],
	 [_,_,_,4,_,_,8,_,5],
	 [7,8,9,_,5,_,_,_,_],
	 [_,_,_,_,_,6,2,_,3],
	 [8,_,1,_,_,_,7,_,_],
	 [_,_,_,1,2,3,_,8,_],
	 [2,_,5,_,_,_,_,_,9]].

t(Rows) :-
    problem(Rows),
    sudoku(Rows),
    maplist(labeling([ff]), Rows),
    maplist(writeln, Rows).

grid(N, Rows) :-
    length(Rows, N),
    maplist(length_n(N), Rows).

length_n(N, L) :-
    length(L, N).

xtranspose([], []).
xtranspose([[]|_], []).
xtranspose(Rows, [Col|Cols]) :-
    first_column(Rows, Col, RestRows),
    xtranspose(RestRows, Cols).

first_column([], [], []).
first_column([[X|Xs]|Rows], [X|Col], [Xs|RestRows]) :-
    first_column(Rows, Col, RestRows).

xall-distinct([]).
xall-distinct([Row|Rows]) :-
    all_diff(Row),
    xall-distinct(Rows).

all_diff([]).
all_diff([X|Xs]) :-
    different(X, Xs),
    all_diff(Xs).

different(_, []).
different(X, [Y|Ys]) :-
    X #\= Y,
    different(X, Ys).
