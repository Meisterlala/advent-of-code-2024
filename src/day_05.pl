:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).
:- use_module(library(portray_text)).
:- use_module(library(lists)).
:- dynamic(after/2).

set_portray_text(enabled, X, X).

% DCG

input((Po, Pr))-->
	page_orderings(Po),
	blanks,
	pages(Pr),
	eos.

page_orderings([(X, Y)|Pos])-->
	number(X),
	"|",
	number(Y),
	(("\n",
			page_orderings(Pos))|"").

pages([Pr|Prs])-->
	page(Pr),
	(("\n",
			pages(Prs))|"").

page([N|Ns])-->
	number(N),
	((",",
			page(Ns))|""). 


% Part 1

part1((Rules, Pages), X) :-
	write(Rules), nl,
	write(Pages), nl,
	
	forall(member((X, Y), Rules), assertz(after(X, Y))),
	convlist(correct_order, Pages, GdPages),
	sum_mid_pages(GdPages, X).

correct_order(Pages, Pages) :-
	forall(subseq(Pages, [X, Y], _), after(X, Y)).
    
sum_mid_pages(Pages, X) :-
	maplist(midpage, Pages, MPs),
	foldl(plus, MPs, 0, X).

midpage(Pages, E) :-
	length(Pages, N),
	Mid is (N + 1)/2,
	nth1(Mid, Pages, E).

% Tests

example("47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47").


?- example(E), string_codes(E, C),
once(phrase(input(I), C, [])), part1(I, 143).
