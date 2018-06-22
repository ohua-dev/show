# ohua-show

A small tool to render ohua compiled graphs using graphviz.

## Installation

Clone the repository and install using [`stack`](https://haskellstack.org).

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
