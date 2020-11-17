# LISP

A mini-LISP interpreter in C++ which itself implements a larger LISP.
Inspired by [The Roots of LISP](http://languagelog.ldc.upenn.edu/myl/llog/jmc.pdf).

The C++ implements a mini-LISP interpreter consisting of:
* `quote`
* `atom`
* `eq`
* `car`
* `cdr`
* `cons`
* `cond`
* `lambda`
* (`set!`)

The mini-LISP interpreter implements a larger LISP additionally containing:
* `quasiquotation`
* (`algebraic data types`)
