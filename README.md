# Liaison

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/liaison.svg?style=flat)](https://hackage.haskell.org/package/liaison)
[![Stackage Nightly](http://stackage.org/package/liaison/badge/nightly)](http://stackage.org/nightly/package/liaison)
[![Stackage LTS](http://stackage.org/package/liaison/badge/lts)](http://stackage.org/lts/package/liaison)
[![Build Status](https://travis-ci.org/mrkkrp/liaison.svg?branch=master)](https://travis-ci.org/mrkkrp/liaison)

> Nix sat in the corner of the bar in her usual state of indifference. And
> then he came in. Haskell. There was something special about this guy. The
> smell of cheap tobacco mixed with Chanel egoïste reminded her of her
> forgotten desires. Desires, which stayed rather unsatisfied with C plus
> plus.
>
> C plus plus… She still remembered him, young and immature, back then she
> used to call him just C. Then C plus. The second plus didn't help either.
> Sometimes it's just not what a girl wants. But with Haskell it was
> different. From the first sight she understood that this guy didn't need
> no pluses, for he himself was the infinite multiplier…

Liaison is a package that allows us to use a subset of Nix language as
general-purpose configuration language. The project so far is just a
playground, I do not expect that people will use it in its current state.

## Get started

* Specification of the language can be found [here](LANG.md).
* Roadmap can be found [here](ROADMAP.md).
* See Haddocks for info about how to use this in Haskell programs.

## Motivation

What we want for writing configuration is something like JSON or YAML, but
with lambdas, and that's exactly what Nix is. In addition to that, subset of
Nix should be more portable as a language than e.g. [YAML][yaml-sucks].

But hey, there is…

* [`hnix`][hnix], but its scope is wider because it aims to re-implement Nix
  in its entirety. `liaison` is simpler and specifically written to work as
  a configuration language. For example, we provide a validation framework,
  special set of combinators to generate configuration files, etc.

* [`dhall`][dhall], which has *too* powerful type system, and is very slow.
  The promise of termination does not give anything from practical point of
  view, because it's possible to write functions that will eventually
  terminate, but not in our lifetimes. `liaison` aims to be fast and simple
  to use. Its dynamic nature is compensated by excelent error messages.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/liaison/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2019 Mark Karpov

Distributed under BSD 3 clause license.

[yaml-sucks]: https://arp242.net/weblog/yaml_probably_not_so_great_after_all.html
[hnix]: https://hackage.haskell.org/package/hnix
[dhall]: https://hackage.haskell.org/package/dhall
