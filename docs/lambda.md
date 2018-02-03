This document describes the design and implementation of `lambda`
expressions and closures in Schism.

There are two main pieces. The first is dealing with the `lambda`
expressions, while the second deals with calls to `lambda`s.

The `lambda` expressions get compiled into closures. These are
represented basically as pairs, but with a different tag. The `car` of
the pair is an index into the indirect call table, and the `cdr` is
the captured environment, represented as a list.

## Closure conversion

First, the bodies of all `lambda`s are lifted to top-level functions. For simplicity,
these all have a uniform calling convention. Consider the the following example:

```
(let ((x 5))
  (lambda (y)
    (+ x y)))
```

This gets lifted into a top-level function as follows:

```
(define (lambda.0 env args)
  (let ((x (car env))
        (y (car args)))
    (+ x y)))
```

Then, the `lambda` expression is replaced by a closure constructor:

```
(let ((x 5))
  (closure 0 x))
```

This provides a number identifying the lambda body as well as all
captured variables (i.e. the free variables from the expression). The
indentifying number ultimately becomes the index into the indirect
call table.

Closure constructors later get lowered to basically a `cons`-list of
the index and all of the captured values, although with a procedure
tag.

## Lambda call expressions

In order to not break or have to completely rewrite what we have so
far, the existing calls will not change. For most of the compiler,
calls to first class procedures will instead be replaced by
`apply-closure` expressions. These eventually get compiled into a Wasm
`call_indirect` instruction.
