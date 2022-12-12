# GHAPPy

Small utility library to help creating documents (e.g. Audit Reports) working
with `Pandoc` and GitHub issues and the GitHub API.

## How to use

There are two main ways of using GHAppy, namely as a library and as an application.

### As a binary

To use GHAppy as a binary there are a few options:

1. First option is cloning the repository, running `cabal install` and then run
   it via `~/.cabal/bin/GHAppy`.

2. Second way is running it via `nix` with `nix run github:mlabs-haskell/GHAppy`
   .

To see an example of the input file necessary for the library have a look at
the provided [./example/report2.yaml](./example/report2.yaml).

### As a library

- The library can be added as a dependency to your project via
`cabal.project`. Example:

```yaml
-- GHAppy
source-repository-package
   type:     git
   location: https://github.com/mlabs-haskell/GHAppy
```

- Next you would define your document using the `freer-monad` api as can be seen
in [the provided example](./app/Example.hs).

- You must have have a GH API key to be able to pull the issues - have a look at
[./run.sh](./run.sh) how this key is provided to the executable.

- Provide the executable with the correct arguments - and you're done.

## Notes and future features

- At the moment the template file used by the Pandoc conversion from `md` to
  `pdf` is hard coded - this will change.

- [x] The documents could be created via parsing a `yaml` config file - thus
  decoupled from building the library. This is being considered as a next
  feature.

## Developing

- we use `pre-commit-hooks` to ensure code quality.

- to ensure formatting and linting, enter a `devShell` by using `nix develop` or
  `direnv` (`direnv allow`)

- each commit will run all formatting and linting checks that are also run by
  the `CI`

- to run the hooks by hand (most of the tools will apply the proposed changes),
  use `pre-commit run --all` (for more information use `pre-commit --help`)
