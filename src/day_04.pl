:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).



% DCG
parse(Map) --> lines(0, Map).

lines(Y, [Line|Lines]) -->
    line(0, Y, Line), "\n",
    { Y2 is Y + 1 },
    lines(Y2, Lines).
lines(Y, [Line]) --> line(0,Y,Line), (""| "\n"), eos.

line(X, Y, [(X, Y, C)]) --> char(C), {C \= '\n'}.
line(X, Y, [(X, Y, C)|Cs]) --> 
    char(C), { C \= '\n' },
    { X2 is X + 1 },
    line(X2, Y, Cs).

char(C) --> [Code], { char_code(C, Code)}.


% Part 1
part1(Map, Sum) :-
    flatten(Map, Flat),
    findall(X, xmas_h(Flat, X), H), length(H, Horizontal),
    write("Horizontal: "), writeln(Horizontal),
    findall(X, xmas_v(Flat, X), V), length(V, Vertical),
    write("Vertical: "), writeln(Vertical),
    findall(X, xmas_d(Flat, X), D), length(D, Diagonal),
    write("Diagonal: "), writeln(Diagonal),
    Sum is Horizontal + Vertical + Diagonal.
    
    
    
xmas(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4)]) :-
    ( 
        (C1 = 'X', C2 = 'M', C3 = 'A', C4 = 'S');
        (C1 = 'S', C2 = 'A', C3 = 'M', C4 = 'X')
    ),
    select((X1, Y1, C1), Map, R1),
    select((X2, Y2, C2), R1, R2),
    select((X3, Y3, C3), R2, R3),
    select((X4, Y4, C4), R3, _).


xmas_h(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4)]) :-
    X1 #= X2 - 1, X2 #= X3 - 1, X3 #= X4 - 1,
    Y1 #= Y2, Y2 #= Y3, Y3 #= Y4,
    xmas(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4)]).

xmas_v(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4)]) :-
    Y1 #= Y2 - 1, Y2 #= Y3 - 1, Y3 #= Y4 - 1,
    X1 #= X2, X2 #= X3, X3 #= X4,
    xmas(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4)]).

xmas_d(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4)]) :-
    ((
        X1 #= X2 - 1, X2 #= X3 - 1, X3 #= X4 - 1,
        Y1 #= Y2 - 1, Y2 #= Y3 - 1, Y3 #= Y4 - 1
    );
    (
        X1 #= X2 - 1, X2 #= X3 - 1, X3 #= X4 - 1,
        Y4 #= Y3 - 1, Y3 #= Y2 - 1, Y2 #= Y1 - 1
    )),
    xmas(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4)]).

% Part 2
part2(Map, Sum) :-
    flatten(Map, Flat),
    findall(X, mas_x(Flat, X), H), length(H, Sum).

    
mas(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4), (X5, Y5, C5)]) :-
    ( 
        C5 = 'A',
        ((C1 = 'M', C2 = 'S'); (C1 = 'S', C2 = 'M')),
        ((C3 = 'M', C4 = 'S'); (C3 = 'S', C4 = 'M'))
    ),
    select((X1, Y1, C1), Map, R1),
    select((X2, Y2, C2), R1, R2),
    select((X3, Y3, C3), R2, R3),
    select((X4, Y4, C4), R3, R4),
    select((X5, Y5, C5), R4, _).

mas_x(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4), (X5, Y5, C5)]) :-
    (
        X1 #= X5 - 1, X2 #= X5 + 1, X3 #= X5 - 1, X4 #= X5 + 1,
        Y1 #= Y5 - 1, Y2 #= Y5 + 1, Y3 #= Y5 + 1, Y4 #= Y5 - 1
    ),
    mas(Map, [(X1, Y1, C1), (X2, Y2, C2), (X3, Y3, C3), (X4, Y4, C4), (X5, Y5, C5)]).
 

% Tests
?- Input = `MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX`,
    phrase(parse(Map), Input),
    part1(Map, 18).

?- Input = `MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX`,
    phrase(parse(Map), Input),
    part2(Map, 9).

solve_part1(Count) :-
    phrase_from_file(parse(Map), "inputs/day04"),
    %write(Map), 
    part1(Map, Count).

solve_part2(Count) :-
    phrase_from_file(parse(Map), "inputs/day04"),
    %write(Map), 
    part2(Map, Count).