:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).
:- use_module(library(portray_text)).
:- use_module(library(lists)).
:- use_module(library(lists)).
:- dynamic(after/2).

set_portray_text(enabled, X, X).

part1(Pages, X) :-
    convlist(correct_order, Pages, GdPages),
    sum_mid_pages(GdPages, X).

correct_order(Pages, Pages) :-
    forall(subseq(Pages, [X, Y], _), after(X, Y)).

sum_mid_pages(Pages, X) :-
    maplist(midpage, Pages, MPs),
    foldl(plus, MPs, 0, X).

midpage(Pages, E) :-
    length(Pages, N),
    Mid is (N + 1) / 2,
    nth1(Mid, Pages, E).

% Task 2

part2(Pages, X) :-
    convlist(bad_order, Pages, BadPages),
    maplist(fix_order, BadPages, GdPages),
    sum_mid_pages(GdPages, X).

bad_order(Pages, Pages) :- \+ correct_order(Pages, Pages).

fix_order(P, P) :- correct_order(P, P), !.
fix_order(P0, P) :-
    append([Prefix, [X, Y], Suffix], P0),
    \+ after(X, Y),
    append([Prefix, [Y, X], Suffix], P1),
    fix_order(P1, P), !.



input(Rules, Pages) -->
    sequence(rule, (blank, \+ blank), Rules),
    blank, blank,
    sequence(sequence(integer, `,`), (blank, \+ eos), Pages),
    blank.

rule(X-Y) --> integer(X), `|`, integer(Y).

% Tests

example("47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47").


?- example(E), string_codes(E, C),
phrase(input(Rules, Pages), C, []), assert_after(Rules), part1(Pages, 143).
