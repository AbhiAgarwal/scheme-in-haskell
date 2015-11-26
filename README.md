# scheme-in-haskell

Implementing Scheme in Haskell. Following [this](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) tutorial.

### Build instructions

1. `cabal install parsec`
2. `ghc parser.hs`
3. `./parser "(a '(quoted (dotted . list)) test)"`