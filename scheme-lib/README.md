This directly contains Schism's implementations of the R6RS Scheme
libraries.

These are separated from the `lib` directory in order to make
bootstrapping with Guile easier. Guile will use its own `rnrs`
implementation, but will share libraries from the `lib` directory with
Schism.
