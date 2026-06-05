# 15PuzzleTUI (15p)

> I'm either gonna learn Haskell or die trying (I am currently in purgatory)

Shout out to [this](https://github.com/benjaminselfridge/fifteen) because it was made first lol.

This project is a terminal based implementation of the [15
Puzzle](https://en.wikipedia.org/wiki/15_puzzle) game. It features a simple TUI
built with [brick](https://hackage.haskell.org/package/brick) and allows input
from keyboard or mouse. You can freely customize the color scheme, size of the
grid and tile type in the settings. The game also saves your top 5 times for
each grid size into a local file (On linux it is commonly
`~/.local/share/15p/leaderboards/`). 

## Build From Source

*This project uses [cabal](https://www.haskell.org/cabal/) to build haskell files.*

```sh
cabal run
```

Or if you want to install the binary to your path permanently

```sh
cabal install
```

## Install & Run

Download the latest release from the [releases
page](https://github.com/theme222/15PuzzleTUI/releases) and run it on the
command line

```sh
./15p
```

### Supported flags

```sh
-h, --help           Show this help menu
-v, --version        Show the current version
-s, --size INT       Specify the starting grid size
           INTxINT   Examples: 4 5x2 3x3
```

## Gallery

These images were taken in game on the [Ghostty](https://ghostty.org/) terminal emulator.

![Game](https://github.com/theme222/15PuzzleTUI/blob/main/images/game.png)

![Settings](https://github.com/theme222/15PuzzleTUI/blob/main/images/settings.png)

## Internals

### Shuffling

This puzzle shuffles any nxm board perfectly randomly using the identity [in which
the parity of the permutation must be even](https://en.wikipedia.org/wiki/15_puzzle#:~:text=In%20particular%2C%20if%20the%20empty%20square%20is%20in%20the%20lower%20right%20corner%2C%20then%20the%20puzzle%20is%20solvable%20only%20if%20the%20permutation%20of%20the%20remaining%20pieces%20is%20even%2E)
when the empty square is in the lower right corner. It then randomizes the
position of the empty square afterwords to generate the final shuffled board.

### Solvers

The program contains a total of 2 solving algorithms accessable by pressing `h` in game.
Press `m` to head into the settings to switch between the two.

#### IDA*

Internally [Iterative Deepening A*](https://en.wikipedia.org/wiki/Iterative_deepening_A*) is implemented with
the heuristic of `Manhattan distance + Linear Conflict`. This allows the
algorithm to find the guaranteed shortest path to the goal state. However this
algorithm struggles with board sizes with more than 15 tiles due to the
nature of the exponentially branching nodes of possible paths.

#### Fringe-Solve

Fringe-Solve is a simple algorithm based on the [fringe solving method](https://www.youtube.com/shorts/yCxhqQs_u3g) that
solves the rows and columns from the top left to the bottom right. It doesn't
guarantee the shortest path (it is actually quite far from it) but it will find
this path much faster with a more intuitive path than `IDA*`. 

### Input Detection

Mouse input detection will only work if the terminal supports sending mouse
events. There is weird quirk in which different terminals send mouse events with
different intervals. Currently I have noticed that the
[kitty](https://sw.kovidgoyal.net/kitty/) terminal emulator sends mouse events
at a much faster rate than other terminals, causing previous releases to lag
having to deal with too many inputs (Since the state of the output was directly
tied to the changes in input events.) This has since been fixed by only updating
the output with a constant refresh rate that can be changed in the settings.
This can actually be helpful if you want to guarantee that none of your mouse
swipes will be skipped.