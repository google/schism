# NOT ACTIVELY MAINTAINED

This repository is no longer actively maintained. Future development should be
directed to https://github.com/schism-lang/schism.

# Schism

Schism is an experimental compiler from Scheme to WebAssembly. It
enables developers to run programs written in Scheme in the browser or
server environments such as NodeJS. The compiler supports a subset of
the R6RS version Scheme, and is self-hosting, meaning Schism is
implemented in Schism itself.

This is not an officially supported Google product.

Development so far has focused on features necessary for
self-hosting. The compiler itself is written in, and compiles, a very
small subset of Scheme. Now that self-hosting has been achieved,
development can shift towards supporting a more complete subset of
Scheme. Just to be clear, by subset we mean that all programs
supported by Schism are also legal R6RS Scheme programs and they will
have the same behavior when run under Schism or an R6RS-compliant
Scheme.

Besides just being fun, one of the goals of this project is to explore different
ways to use WebAssembly. This includes implementing garbage collection, possibly
using the WebAssembly GC proposal, dynamic linking and code generation, etc.

Chez Scheme was used to bootstrap, but development no longer relies on
an existing Scheme implementation and can instead run purely based on
snapshots checked into the repository.  The compiler can also bootstrap
from GNU Guile.

As mentioned, the goal has been to prioritize features needed for self
hosting. Here are some of the current restrictions:

* No use of `syntax-case`, `syntax-rules`, or `define-syntax`.
* Only one file to start with, so we don't have to figure out how to link
  multiple libraries.
* Only use `define` to create top level functions and variables.
* Only fixed arity functions can be defined, forms such as `(define (f
  x . args ...)` are not supported.
* Use a small amount of syntax, because we won't have a proper macro expander at
  first. There is a pass to expand some of the simpler and more useful macros.
* Restrict data types and operations on those. For now, we can use:
  * Booleans
  * Numbers (integers within the `int32` range)
  * Characters
  * Pairs
  * Strings
  * Symbols

As more features are supported by the compiler, we will remove these
restrictions.

See the `docs` directory for more information about how various
features are implemented.

## Current Status and Next Steps

The compiler is self-hosting! Now the goal is to make a more complete
language. Some of the big missing features are:

* Variable length argument lists
* Macros
* Support for multiple files and modules
* Ports

## Testing

We currently use a very simple testing protocol. The `test/` directory includes
a number of Scheme libraries, each of which export a function called
`do-test`. This function can do whatever it wants, but it must return a value
other than `#f` to pass.

To run all the tests, do `./run-tests.sh`.

## Schism uses experimental WebAssembly features

Note!  One of the purposes of Schism is to advance the state of the art
in WebAssembly implementations.  Currently, the WebAssembly emitted by
Schism uses the following experimental features:

* [Reference types](https://github.com/WebAssembly/reference-types/blob/master/proposals/reference-types/Overview.md)
* [Tail calls](https://github.com/WebAssembly/tail-call/blob/master/proposals/tail-call/Overview.md)

As of August 2019, the only production WebAssembly implementation that
has both of these features is V8, and both features are behind a flag.
To use the features with Node, we add the
`--experimental-wasm-return-call` and `--experimental-wasm-anyref` flags
to Node's argument list.  We hope to improve this situation in the
future.

## The Playground

This repository includes a very simple `playground.html`, which gives a
lightweight way to play around with the compiler. Be warned, there is
almost no error checking right now, so strange things can happen.

The best way to use it is to start up a web server (`python -m
SimpleHTTPServer` should work) and point your browser at the page.

Note that because Schism uses experimental WebAssembly features, you
need a browser that supports these features.  To get a Chrome that has
these features, try:

```
chrome --js-flags="--experimental-wasm-anyref --experimental-wasm-return-call"
```

Then navigate to `http://localhost:8000/playground.html`.

## Development

The compiler code all lives in `schism/compiler.ss`. There is a
JavaScript runtime in `rt/rt.mjs`. The goal is to keep as much code as
possible in Scheme, but the runtime is needed to interact with the rest
of the world. Also, until larger parts of the [GC
proposal](https://github.com/WebAssembly/gc/blob/master/proposals/gc/Overview.md)
are implemented in WebAssembly engines, the run-time also has some
routines to allocate memory using the host (usually the JavaScript GC),
to enable garbage collection. In the future, the runtime should also
handle dynamic module loading.

We have a staged build system. Stage0 is the compiler snapshot, stored in
`schism-stage0.wasm`. Stage1 is generated by compiling `schism/compiler.ss` with
the Stage0 compiler, Stage2 by compiling with the Stage1 compiler, and Stage3 by
compiling with the Stage2 compiler. Stage3 should be equal to the Stage2
compiler, and currently we only generate it to verify this is the case.

To add a new feature, the usual flow is to start by adding a small test that
uses it. This test will probably fail at least the Stage0 compiler. Once the
feature is implemented, then the Stage1 and Stage2 compilers should pass the
test. Note that you cannot use the new feature in the compiler until it works in
stage2. Once this happens, you should make a new snapshot using `mksnapshot.sh`.
