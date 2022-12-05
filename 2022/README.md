# Advent of Code 2022

Language: Haskell

For this I used GHCup on Windows to install GHC 9.4.2 and Cabal 3.8.1.0.

## Running

Run the following at the command line from within the `2022` directory:

```bash
cabal repl
```

Each puzzle will be loaded into GHCI. Each puzzle is runnable by calling the function `pNa` for
part A of each day's puzzle or `pNb` for part B. In both cases, `N` is a day number from 1-24.

Each puzzle function takes one parameter that is either `Example` or `Actual`. `Example` runs the
puzzle's solution against the example input, and `Actual` runs it against the input given for my
user. All puzzle inputs are accessible from the `data` folder.
