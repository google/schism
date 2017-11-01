# Schism

Schism is an experimental compiler from a subset of R6RS Scheme to
WebAssembly. The goal is that the compiler will be self-hosting. This leads to a
development style that makes use of a small portion of R6RS Scheme and
prioritizes implementing the features needed to self-host. Once self-hosting is
achieved, new features can be added as its convenient.

Besides just being fun, one of the goals of this project is to explore different
ways to use WebAssembly. This includes implementing garbage collection, possibly
using the WebAssembly GC proposal, dynamic linking and code generation, etc.

Chez Scheme is used for development, but other Schemes will probably work too.

As mentioned, the goal is to prioritize features needed for self hosting. A
corollary to this is that Schism will use a greatly reduced subset of Scheme
until it is self-hosted. Here are some of the current restrictions:

* No `lambda`, no closures.
* Functions may only be created with `define`, and particularly only the
  `(define (foo args ...) body)` form.
* No use of `syntax-case`, `syntax-rules`, or `define-syntax`.
* Only one file to start with, so we don't have to figure out how to link
  multiple libraries.
* Only use `define` to create functions, not global variables or objects.
* Use a small amount of syntax, because we won't have a proper macro expander at
  first. There will be a pass to expand some of the simpler and more useful
  macros.
* Restrict data types and operations on those. For now, we can use:
  * Numbers
  * Symbols
  * Pairs
  * Bytevectors (basically only for generating the final .wasm bytes)
  * Strings

As more features are supported by the compiler, we will remove these
restrictions.

## Testing

We currently use a very simple testing protocol. The `test/` directory
includes a number of Scheme libraries, each of which export a function
called `do-test`. This function can do whatever it wants, but then
must return 1 if the test passes and 0 otherwise. Once booleans are
supported, these tests will return `#f` on failure.

