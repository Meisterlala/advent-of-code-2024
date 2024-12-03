:- use_module(library(portray_text)).
set_portray_text(enabled, X, X).

%DCG
memory(Ms) --> 
    ignored,
    multiplications(Ms),
    ignored.

multiplications([M|Ms]) -->
    multiplication(M),
    multiplications_r(Ms).

multiplications_r([M|Ms]) -->
    multiplication(M),
    multiplications_r(Ms).
multiplications_r(Ms) -->
    [_],
    multiplications_r(Ms).
multiplications_r([]) --> [].

multiplication(X*Y) --> 
    "mul(",
    number(X),
    ",", 
    number(Y),
    ")".

number(N) -->
    digits(D),
    { number_codes(N, D) }.

digits([D1]) -->
    digit(D1).
digits([D1, D2]) -->
    digit(D1),
    digit(D2).
digits([D1, D2, D3]) -->
    digit(D1),
    digit(D2),
    digit(D3).

digit(D) -->
    [D],
    { code_type(D, digit) }.

ignored --> [].
ignored --> [_], ignored.


% DCG 2
memory2(Ms) --> 
    ignored,
    multiplications2l(enabled, Ms),
    ignored.

multiplications2l(State, [M|Ms]) -->
    multiplication(M),
    multiplications2(State, Ms).

multiplications2(_, [M|Ms]) -->
    do, 
    multiplications2(enabled, [M|Ms]).
multiplications2(_, [M|Ms]) -->
    dont,
    multiplications2(disabled, [M|Ms]).
multiplications2(enabled, [M|Ms]) -->
    multiplication(M),
    multiplications2(enabled, Ms).
multiplications2(State, Ms) -->
    [_],
    multiplications2(State, Ms).
multiplications2(_, []) --> [].
    
do --> "do()".
dont --> "don't()".


% Part 1
part1(Memory, Sum) :- 
    multiply(Memory, Sum).

multiply([], 0).
multiply([X*Y|T], Sum) :-
    multiply(T, Sum1),
    Sum is X * Y + Sum1.

% Part 2
part2(Memory, Sum) :- 
    multiply(Memory, Sum).


% Test
example_input(`xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))`).


test_parse(M, Rest) :- example_input(In),
    phrase(memory(M), In, Rest).

?- example_input(In),
    once(phrase(memory(M), In, [])),
    M = [2*4, 5*5, 11*8, 8*5].

?- I2 = `xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))`,
    once(phrase(memory2(M), I2, [])),
    M = [2*4, 8*5].

?- example_input(In),
    once(phrase(memory(M), In, Rest)),
    Rest = [],
    part1(M,161).

?- I2 = `xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))`,
    once(phrase(memory2(M), I2, Rest)),
    Rest = [],
    part2(M,48).

solve_part1(Sum) :-
    once(phrase_from_file(memory(M), "inputs/day03")),
    part1(M, Sum).

solve_part2(Sum) :-
    once(phrase_from_file(memory2(M), "inputs/day03")),
    part2(M, Sum).
    
