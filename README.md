# 15PuzzleTUI (15p)

> I'm either gonna learn Haskell or die trying (I am currently on the brink of death)

Shout out to [this](https://github.com/benjaminselfridge/fifteen) because it was made first lol.

This project is a terminal based implementation of the 15 Puzzle game. It
features a simple TUI built with
[brick](https://hackage.haskell.org/package/brick) and allows input from
keyboard or mouse. You can freely customize the color scheme and size of the
puzzle in the settings. The game also saves your top 5 times into a local file
(On linux it is commonly `~/.local/share/15p/leaderboards/`).

## Build & Run From Source

*This project uses [cabal](https://www.haskell.org/cabal/) to build haskell files.*

```sh
cabal run
```

Or if you want to install the binary to your path permanently

```sh
cabal install
```

## Run

Currently it is only a tui so there is no available flags like `-h` or `-v`.
Download the latest release from the [releases
page](https://github.com/theme222/15PuzzleTUI/releases) and run it on the
command line by simply typing `./15p`.

## Gallery

These images were taken in game on the [Ghostty](https://ghostty.org/) terminal emulator.
