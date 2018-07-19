# A visualizer for Ohua graphs

[![Build Status](https://travis-ci.org/ohua-dev/show.svg?branch=master)](https://travis-ci.org/ohua-dev/show)
[![Latest Release](https://img.shields.io/github/release/ohua-dev/show.svg)](https://github.com/ohua-dev/show/releases/latest)

A small tool to render ohua compiled graphs using graphviz.

## Usage

Invoke `ohua-show --help` see all options and commands. Help for subcommands can
be found by runnning `ohua-show <subcommand> --help`. Be advised that options
and arguments listed for `ohua-show` itself work on all subcommands. Arguments
listed tehre should be prepended to the arguments for the subcommands.

The `preview` subcommand may exit withoput doing anything, I don't know why,
perhaps it can't find some program it needs for that. In that case I'd recommend
simply rendering to PDF using `ohua-show print -f Pdf <input-graph.ohuao>`.

To run `print` with a custom format (as supplied by `-f`) the `graphviz`
executable must be installed on the system. If no format is supplied the output
will be a `dot` file. This works even if `graphviz` is not installed.

## Installing

Prebuilt binaries are available for 64bit Linux and OSX. Provided for each
release by [Travis CI](https://travis-ci.org/ohua-dev/show).

Simply download the appropriate executable for you platform (`ohua-show-linux` or
`ohua-show-osx`) from the [releases page](/releases/latest).

## Building from source

Required tools:

- `stack` https://haskellstack.org

### Instructions

1. Clone the repository

    `git clone https://github.com/ohua-dev/ohuac`

2. Build the program

   `stack install`

   This downloads and builds all dependencies, as well as the Haskell compiler
   `ghc`, should it not be present already. It should not interfere with any
   system-level libraries, Haskell ecosystems etc.

   It builds the executable `ohua-show` and copies it to `~/.local/bin`. If you
   do not wish this use `stack build` instead and find the path of the binary
   using `stack exec -- which ohua-show` afterwards
