# sandalo.dev

Generates the static site found at <https://sandalo.dev>.

Built using Haskell and [Hakyll](https://jaspervdj.be/hakyll/).

## Installing dependencies

This project requires the [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/) and the [Stack](https://docs.haskellstack.org/en/stable/) tool to build and run. Both can be installed using [GHCup](https://www.haskell.org/ghcup/).

To install GHCup on Linux, run the following command:

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

For other platforms or the latest information, check out the [GHCup website](https://www.haskell.org/ghcup/).

## Building and running the program

The `sandalo-dev` project described in `package.yaml` builds the `site` binary, which is responsible for building and serving the static website.

To build and run `site`, use the following command:

```
stack run
```

This will show the available commands for the `site` binary. Some useful commands are as follows:

```
# Builds the website to `./site`
stack run build

# Builds the website and serves it to localhost:8080,
# watching for changes in the non-Haskell files
stack run watch
```
