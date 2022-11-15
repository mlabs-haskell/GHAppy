# GHAPPy

Utility to retrieve a list of tickets from a GH repository you have access to
and save them as pdfs.

## To run

1. You have to have a GH API key - have a look at `./run.sh` how this key is
provided to the binary.

## Developing

- we use `pre-commit-hooks` to ensure code quality
- to ensure formatting and linting, enter a `devShell`
  by using `nix develop` or `direnv` (`direnv allow`)
- each commit will run all formatting and linting
  checks that are also run by the `CI`
- to run the hooks by hand (most of the tools will apply the
  proposed changes), use `pre-commit run --all` (for more
  information use `pre-commit --help`)
