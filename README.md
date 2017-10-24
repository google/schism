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
