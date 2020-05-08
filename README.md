# reflex-vty [![hackage](https://img.shields.io/hackage/v/reflex-vty.svg)](https://hackage.haskell.org/package/reflex-vty) [![hackage-ci](https://matrix.hackage.haskell.org/api/v2/packages/reflex-vty/badge)](https://matrix.hackage.haskell.org/#/package/reflex-vty) [![travis-ci](https://api.travis-ci.org/reflex-frp/reflex-vty.svg?branch=develop)](https://travis-ci.org/reflex-frp/reflex-vty)

## A library for building functional reactive terminal applications

![reflex-vty example animation](doc/welcome.gif)

Feature requests, pull requests, and other feedback are welcome and appreciated (see the [contribution guide](CONTRIBUTING.md)). This library
is still experimental, so big changes are possible!
### How to Build

#### With reflex-platform

Enter a nix-shell for the project:
```bash
git clone git@github.com:reflex-frp/reflex-platform
git clone git@github.com:reflex-frp/reflex-vty
cd reflex-vty
../reflex-platform/scripts/work-on ghc ./.
```

From within the nix-shell you can:
* Run the example: `cabal repl example`
* Load the library in the repl: `cabal repl reflex-vty`
* Build the example executable: `cabal build example`
* Build the docs: `cabal haddock`
* Run ghcid for immediate compiler feedback when you save a .hs file: `ghcid -c "cabal repl reflex-vty --ghc-options=-Wall"`
* etc.

#### With cabal

Please see the `tested-with` field of the cabal file for known-compatible versions of GHC.

From the reflex-vty project directory:

```bash
# nix-shell -p cabal-install binutils icu # for nix users
cabal new-configure
cabal new-build # to build the library and example
cabal new-repl # to enter a repl for the library
cabal new-repl example # to enter a repl for the example executable
```
