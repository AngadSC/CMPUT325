% Angad Chahil
% ID- 1757558
% CCID - achahil
% CMPUT 325

% Question 1 Part A
% We have all the marks we need to concert into their weights and add together 
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
    WeightedScore is (Mark / Max) * Percentage.


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


% Question 1 Part C
% update one mark 

query3(Semester, Name, Type, NewMark) :-
    c325(Semester, Name, As1, As2, As3, As4, Midterm, Final), !,
    retract(c325(Semester, Name, As1, As2, As3, As4, Midterm, Final)),
    updated_record(Type, NewMark, As1, As2, As3, As4, Midterm, Final,NewAs1, NewAs2, NewAs3, NewAs4, NewMidterm, NewFinal ),
    assertz(c325(Semester, Name, NewAs1, NewAs2, NewAs3, NewAs4, NewMidterm, NewFinal))

query3(_, _, _, _) :-
    write('record not found'), nl.

updated_record(as1, NewMark, _, As2, As3, As4, Midterm, Final, NewMark, As2, As3, As4, Midterm, Final).
updated_record(as2, NewMark, As1, _, As3, As4, Midterm, Final, As1, NewMark, As3, As4, Midterm, Final).
updated_record(as3, NewMark, As1, As2, _, As4, Midterm, Final, As1, As2, NewMark, As4, Midterm, Final).
updated_record(as4, NewMark, As1, As2, As3, _, Midterm, Final, As1, As2, As3, NewMark, Midterm, Final).
updated_record(midterm, NewMark, As1, As2, As3, As4, _, Final, As1, As2, As3, As4, NewMark, Final).
updated_record(final, NewMark, As1, As2, As3, As4, Midterm, _, As1, As2, As3, As4, Midterm, NewMark).

    
