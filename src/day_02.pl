use_module(library(pio)).


% DCG
file_content(Levels) --> levels(Levels).

levels([]) --> [].
levels([L1|Ls]) --> level(L1), ([] | " "),"\n",  levels(Ls).

level(R) --> reports(R).

reports([R|Rs]) --> report(R), " ", reports(Rs).
reports([R]) --> report(R).

report(R) --> digits(D), { number_codes(R, D) }.

digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).

digit(D) --> [D], { code_type(D, digit) }.

% Part 1
part1(Levels, Safe):-
    findall(X, include(save_report, Levels, X), [Reports]),
    length(Reports, Safe).
    
save_report(R) :-
    (report_increasing(R)
    );    (report_decreasing(R)).
    

report_increasing([_]).
report_increasing([R1, R2|T]) :-
    R1 < R2,
    R2 - R1 =< 3,
    report_increasing([R2|T]).

report_decreasing([_]).
report_decreasing([R1, R2|T]) :-
    R1 > R2,
    R1 - R2 =< 3,
    report_decreasing([R2|T]).


% part 2
part2(Levels, Safe):-
    findall(X, (
        member(X, Levels),
        findall(V, variation(X, V), Variants),
        member(Variant, Variants),
        save_report(Variant)
    ), Reports),
             
    sort(Reports, Sorted),
    length(Sorted, Safe).

variation(Report, Variation) :-
    Variation = Report;
    select(_, Report, Variation).


% Test cases
example_input("7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9\n").
?-  example_input(S), 
    string_codes(S, Codes),
    phrase(file_content(L), Codes),
    L = [[7, 6, 4, 2, 1], [1, 2, 7, 8, 9], [9, 7, 6, 2, 1], [1, 3, 2, 4, 5], [8, 6, 4, 4, 1], [1, 3, 6, 7, 9]].

?-  example_input(S),
    string_codes(S, Codes),
    phrase(file_content(L), Codes),
    part1(L, 2).

?-  example_input(S),
    string_codes(S, Codes),
    phrase(file_content(L), Codes),
    part2(L, 4).

?-  string_codes("90 91 93 96 93\n", Codes),
    phrase(file_content(L), Codes),
    L = [[90, 91, 93, 96, 93]].

solve_part1(Safe) :-
    phrase_from_file(file_content(Levels), "../inputs/day02"),
    part1(Levels, Safe).


solve_part2(Safe) :-
    phrase_from_file(file_content(Levels), "../inputs/day02"),
    part2(Levels, Safe).
