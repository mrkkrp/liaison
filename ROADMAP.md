# Roadmap

This describes stages of development for the project. The main idea is to
make development iterative and get a something functional very soon and then
just add features one by one when I'm in the right mood. Given that I have
little time for the project, it's essential that I don't embark on something
that will require considerable efforts before it becomes useful.

## 0.0.1.0

Basic feature set, only tiny subset of language is implemented, but all
components and documentation are in place.

* Configuration [language features][nix-language]:
  * String literals in simplest form, no antiquotation, no multiline syntax,
    no special handling of URIs
  * Numeric literals: integers and floats
  * Set literal, but no attribute selection or `or` operator, no support for
    recursive sets.
* Implement all functionality around the selected feature set.
* Tests.
* Proper readme explaining wtf.
* Documentation of language itself `LANG.md` file.

## 0.0.2.0

* Add URIs.
* Add Booleans.
* Add `null` value.

## 0.0.3.0

* Add paths, all features except for `<nixpkgs>` syntax (which may be
  totally inapplicable for this library).
* List literal.
* Comments: sinlgle-line and multi-line.

## 0.0.4.0

* Antiquotataion in strings but still no multiline strings.
* `let` expressions.

## 0.0.5.0

* Recursive sets with detection of infinite recursion.

## 0.0.6.0

* Inheriting attributes in sets.
* `if` expressions.

## 0.0.7.0

* Assertions.
* `with` expressions.

## 0.0.8.0

* Implement [all operators][operators], their evaluation, etc.

## 0.0.9.0

* Functions (lambda abstraction) disallowing recursive invocations though.

## 0.0.10.0

* Some built-in functions, not necessarily the same functions that Nix has
  though. Determine when we get to this.

## 0.1.0.0

* Minor corrections, etc.
* Add parser benchmarks.
* Announce.

[nix-language]: https://nixos.org/nix/manual/#ch-expression-language
[operators]: https://nixos.org/nix/manual/#sec-language-operators
