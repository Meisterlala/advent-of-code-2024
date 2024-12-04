# Advent of Code 2024 Solutions in Prolog  <img src="./.github/swi-prolog.svg" align="right" width="350">

[![LoC](https://tokei.rs/b1/github/Meisterlala/advent-of-code-2024)](https://github.com/search?q=repo%3AMeisterlala%2Fadvent-of-code-2024++language%3AProlog&type=code)  [![GitHub last commit (branch)](https://img.shields.io/github/last-commit/meisterlala/advent-of-code-2024/main)](https://github.com/Meisterlala/advent-of-code-2024/commits/main/)

This repository contains solutions for the [Advent of Code 2024](https://adventofcode.com/) challenges implemented in Prolog. The solutions are designed to read input data from text files, process the data, and compute the required results for each challenge.

## What is Advent of Code? 🎄

[Advent of Code](https://adventofcode.com/) is an Advent calendar of small programming puzzles for a variety of skill sets and skill levels that can be solved in any programming language you like. Each day there will be a new problem that is split into 2 parts. The second parts is a more difficult variation of the first part. This repository contains my solutions to the Advent of Code 2024 puzzles


## Solutions

| Day                                                                            | Stars         |         Code          |
| :----------------------------------------------------------------------------- | :------------ | :-------------------: |
| [Day 1: Historian Hysteria](https://adventofcode.com/2024/day/1)               | :star: :star: | [Code](src/day_01.pl) |
| [Day 2: Red-Nosed Reports](https://adventofcode.com/2024/day/2)                | :star: :star: | [Code](src/day_02.pl) |
| [Day 3: Mull It Over](https://adventofcode.com/2024/day/3)                     | :star: :star: | [Code](src/day_03.pl) |
| [Day 4: Ceres Search](https://adventofcode.com/2024/day/4)                     | :star: :star: | [Code](src/day_04.pl) |

## How to Run the Code

Install [SWI-Prolog](https://www.swi-prolog.org/download/stable) on your machine to run the solutions.
Then log into the Advent of Code website and download the input data for the specific day you want to run the code for. Save the input data in a file named `dayXX` in the `input` directory, where `XX` is the day number.

Then run:
```sh
swipl
?- [day_01].  % Load the day_01.pl file
?- solve_part1(Part1_res).
?- solve_part2(Part2_res).
```
