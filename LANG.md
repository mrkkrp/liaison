# Liaison language

Liaison is a strict subset of [Nix language](nix-language). This document
describes features we support so far.

## Values

### Simple values

Liaison has the following basic data types:

* *Strings* are enclosed between double quotes, e.g., `"foo bar"`. Strings
  can span multiple lines. The special characters `"` and `\` and the
  character sequence `${` must be escaped by prefixing them with a backslash
  `\`. Newlines, carriage returns and tabs can be written as `\n`, `\r` and
  `\t`, respectively.

* Numbers, which can be integers (like `123`) or floating point (like
  `123.43` or `.27e13`).

### Sets

Sets are just a list of name/value pairs (called attributes) enclosed in
curly brackets, where each value is an arbitrary expression terminated by a
semicolon. For example:

```nix
{ x = 123;
  text = "Hello";
  y = { bla = 456; };
}
```

This defines a set with attributes named `x`, `text`, `y`. The order of the
attributes is irrelevant. An attribute name may only occur once.

You can use arbitrary double-quoted strings as attribute names:

```nix
{ "foo" = 123; "nix-1.0" = 456; }
```

[nix-language]: https://nixos.org/nix/manual/#ch-expression-language
