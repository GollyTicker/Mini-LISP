# LISP

A mini-LISP interpreter in C++ which itself implements a larger LISP.
Inspired by [The Roots of LISP](http://languagelog.ldc.upenn.edu/myl/llog/jmc.pdf).

The C++ implements a mini-LISP interpreter consisting of:
* `quote`
* `atom`
* `eq` (atom equality and empty list equality)
* `car`
* `cdr`
* `cons` (and it's abbreviation `list`)
* `cond`
* `lambda`
* `define!` (recursive binding)
* `environment` (returns all bindings to re-direct standard library)
* `+` and `decr` (convenience for example numerical functions)

The primitives satisfy following equations:
* *TODO*

The mini-LISP interpreter implements a larger LISP additionally containing:
* `quasiquotation`
* (`algebraic data types`)
