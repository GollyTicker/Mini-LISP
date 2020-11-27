# Mini-LISP

A mini-LISP interpreter in C++ with a small standard library and an `eval` function which interprets same LISP expressions. [Try it online here!](https//not-implemented-yet.sorry)

The purpose of this project was to develop a better understanding of some key features of LISP (e.g. [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity)) by writing an interpreter for it - and because it's fun to do so! I choose C++ since I wanted to improve my C++ skills - and because chosing simple constructs from C++ enables a better understanding than using higher level abstractions (e.g. Python objects). Writing an interpreter also allows one to experiment with variations which are not implemented in standard implementations of LISP. The interpreter is slightly different than comparable full implementations of LISP such as GNU clisp and these are documented below. During implementation I was mostly going along the lines of [The Roots of LISP](http://languagelog.ldc.upenn.edu/myl/llog/jmc.pdf).

### mini-LISP interpreter in C++
The interpreter comes with the following primitives:
* `quote`
* `atom`
* `eq`
* `car`
* `cdr`
* `cons` (and it's abbreviation `list`)
* `cond`
* `lambda`
* variable binding with `define` + global definitions with `define!`
* `environment` (returns all global global definitions)
* `+` and `decr` (convenience for example numerical functions)
* `quasiquotation` (?)

### TODOs
* package into REST api
* serve via Vue
* run in cloud via Digital Ocean
* give examples of code and give small cheat sheet for what is implemented
* add *homoiconicity* example
  * access implementations via `(define ast my-func ...)` e.g. `(define ast null ast) => (lambda (x) (eq x (quote ())))`
    * should we also show implementations for primitives like `cond` etc?
    * shall we enable changing them?
  * use functions and quasiquotation to create computed expressions

### Features
* a function `eval` written in MiniLISP which can emulates MiniLISP itself (in `standard_library.lisp`)
* *memory management* of LISP expressions via C++ `shared_ptr` in `0-AST.cpp`
* *garbage collection* of memoised but unreferenced expressions
* *memoisation* of the evaluated from of expressions via `weak_ptr` in `0-Eval.cpp`. Memoisation is possibe, since all lisp ASTs are mere expressions which don't change state. (`define!` is only meant for top-level bindings)
  * in long running operations 13% to 18% of all expressions were observed to be looked up instead of recomputed
  * this optimisation isn't implemented in the interpreter `eval` written in MiniLISP itself, due to the lack of efficient map datastructures. These expressions run long and are excluded from tests.
* *head-first evaluation*: in expressions where the head itself is a complex expression, the head is evaluated first until it can be reduced to a primitive or lambda function. This enables us to write complex functions where the head can be computed.
  * for example `((cond ('() '+) ('t 'car)) '(1 2 3))` can be run with our interpreter and evaluates to `1` as it choses to run the head `car`
  * other online interpreters such as [this one](https://rextester.com/l/common_lisp_online_compiler) (GNU clisp) cannot evaluate such expressions, as the head always has to be a lambda function or a defined function.
* *lazy variable bindings* in lambdas enabling us to define recursive functions using logic-combinators
  * for example, we can run `(lambda (f x) (f f x)) '(lambda (f n) (cond ((eq n '0) '0) ('t (+ n (f f (decr n)))))) '3)` to compute the sum of the first `n=3` integers. it corresponds to the lambda expression `(λf. λx. f f x) (λf. λn. if n == 0 then 0 else n + (f f (n-1)))`
  * Such definitions are not possible in GNU clisp due to it's head being itself a function
* a small *standard library* can be found in `standard-library.lisp`

### Installation & Usage
1. Please ensure you have a linux sysem with [Docker](https://docs.docker.com/get-docker/) installed
2. Clone this repository
3. Run ```./repl.sh``` to build the docker image and run the MiniLISP REPL
4. To check that all tests pass: `run-tests.sh`

### Example expressions

