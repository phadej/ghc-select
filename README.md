# ghc-select

Helper to switch hvr-ppa ghc and cabal versions (on Ubuntu!)

```sh
$(ghc-select ghc-8.0.1 cabal-1.24 alex-3.1.7 happy-1.19.5)
```

## Installing

Using [*stsck*](http://haskellstack.org/) ([installing](http://docs.haskellstack.org/en/stable/install_and_upgrade.html#ubuntu))

```sh
git clone git@github.com:phadej/ghc-select.git
cd ghc-select
stack install
```
