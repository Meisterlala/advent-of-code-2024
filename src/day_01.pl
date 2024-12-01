part1(L1, L2, Sum):-
    msort(L1, L1_sorted),
    msort(L2, L2_sorted),

    diff(L1_sorted, L2_sorted, Diff),
    sum_list(Diff, Sum).

part2(L1, L2, Simmilarity):-
    findall(Sim, (member(X, L1), simmilarity(X, L2, Sim)), Sim_lists),
    write('Sim lists: '), writeln(Sim_lists),
    
    sum_list(Sim_lists, Simmilarity).

simmilarity(N, L1, Simmilarity):-
    count_occurrences(L1, N, Ns),
    Simmilarity = N * Ns.

% Element wise difference
diff([], [], []).
diff([H1|T1], [H2|T2], [Diff| Res]):-
    Diff is abs(H1 - H2),
    diff(T1, T2, Res).

count_occurrences([], _, 0).
count_occurrences([H|T], H, N) :-
    count_occurrences(T, H, N1),
    N is N1 + 1.
count_occurrences([H|T], X, N) :-
    H \= X,
    count_occurrences(T, X, N).


% DCG to parse the file content
file_content(List1, List2) --> lines(List1, List2).

lines([], []) --> [].
lines([N1|List1], [N2|List2]) --> line(N1, N2), lines(List1, List2).

line(N1, N2) --> number(N1), "   ", number(N2), "\n".

number(N) --> digit(D), digits(Ds), { number_codes(N, [D|Ds]) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([]) --> [].

digit(D) --> [D], { code_type(D, digit) }.

% Read the file and generate two lists
read_file(File, List1, List2) :-
    open(File, read, Stream),
    read_string(Stream, _, Content),
    close(Stream),
    string_codes(Content, Codes),
    phrase(file_content(List1, List2), Codes).

% Example usage
generate_lists_from_file(File, List1, List2) :-
    read_file(File, List1, List2).

% Test cases
?- part1([3,4,2,1,3,3], [4,3,5,3,9,3], 11).
?- part2([3,4,2,1,3,3], [4,3,5,3,9,3], 31).
?- msort([3,4,2,1,3,3], L1), msort([4,3,5,3,9,3], L2), diff(L1, L2, [2,1,0,1,2,5]).

solve_part1(Sum) :- 
    generate_lists_from_file('../inputs/day01', List1, List2),
    part1(List1, List2, Sum).

solve_part2(Simmilarity) :-
    generate_lists_from_file('../inputs/day01', List1, List2),
    part2(List1, List2, Simmilarity).
