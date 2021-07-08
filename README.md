# Mini-LISP

A mini-LISP interpreter in C++ with a small standard library and an `eval` function which interprets same LISP expressions. [Try it online here!](http://swaneet.eu/minilisp)

The purpose of this project was to develop a better understanding of some key features of LISP (e.g. [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity)) by writing an interpreter for it - and because it's fun to do so! I choose C++ since I wanted to improve my C++ skills - and because chosing simple constructs from C++ enables a better understanding than using higher level abstractions (e.g. Python objects). Writing an interpreter also allows one to experiment with variations which are not implemented in standard implementations of LISP. The interpreter is slightly different than comparable full implementations of LISP such as GNU clisp and these are documented below. During implementation I was mostly going along the lines of [The Roots of LISP](http://languagelog.ldc.upenn.edu/myl/llog/jmc.pdf). Finally, I exposed the interpreter in a [scratchpad environment](http://swaneet.eu/minilisp) to make it better accessible.

### Mini-LISP interpreter in C++
The interpreter comes with the following primitives:
* `quote`
* `atom`
* `eq`
* `car`
* `cdr`
* `cons` (and it's abbreviation `list`)
* `cond`
* `lambda`
* `define` + global `define!`
* `environment`
* `+` and `decr`

Examples for the usage of the primitives and a standard-library of functions can be found in section [Example expressions](#example-expressions). The standard-library implementation can be found [here](https://github.com/GollyTicker/Mini-LISP/blob/main/1-interpreter/standard-library.lisp).

### Definitions
* `(quote x)` returns `x`
* `(atom x)` is `t` if and only if `x` is an atom
* `(eq x y)` is `t` if and only if `x` and `y` are equal atoms or both the empty list `()`
* `(car xs)` / `(cdr xs)` return the head / tail of the list `xs`
* `(cons x xs)` creates a new list with head `x` and tail `xs`
  * `(list a b ... z)` is shortform for `(cons a (cons b (... (cons z '()) ...)))`
* `((lambda (var1 ... varN) body) arg1 ... argM)` with `N` <= `M` is evaluated by sequentially evaluating the arguments `arg1 ... argN` and replacing every occurence of `var1` to `varN` in `body` with the evaluated forms of the arguments. `var1` to `varN` are atoms.
* `(define k v body)` replaces every occurence of `k` in `body` with the evaluated form of `v`.
  * `(define! k v)` binds the evaluated form of `v` to the atom `k` globally. This should **only** be used for top-level definitions! It returns with the evaluated form of `v`
* `environment` returns an association list of of all current bindings
* `(+ x1 ... xN)` interprets all atoms `x1` to `xN` as integers and returns their sum as an atom
  * `(decr n)` returns `n-1` as an atom

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
* we can easily *rewrite expressions*. The following code snippet demonstrates this.
```
(define! expr '(+ '1 '2 '3))         ; => (+ (quote 1) (quote 2) (quote 3))
(eval expr '())                      ; => 6
(define! expr (set-head 'list expr)) ; => (list (quote 1) (quote 2) (quote 3))
(eval expr '())                      ; (1 2 3)
```

### Installation & Usage
1. Please ensure you have a linux system with [Docker](https://docs.docker.com/get-docker/) installed
1. (Install cmake for Makefile)
1. Clone this repository
1. Run ```make repl``` to build the docker image and run the MiniLISP REPL locally
1. Run ```make docker-scratchpad``` to run a local frontend at `http://localhost:80`
1. To check that all tests pass: `make test`

To deploy locally, run `./restart-service.sh`.

For further debugging purposes, one can use `make docker-bash` to build and connect to a terminal in the container. The server can be run via `make docker-server` at the port in `3-HTTP/port.txt`.

To debug locally:
* in Makefile comment out the build dependency `save-backend-address` for the rule `build`
* set `publicPath` in vue.config.js to `/`
* set `3-HTTP/http-backend-address.txt` to `localhost`
* set `3-HTTP/http-backend-path` to an empty string
* set `3-HTTP/public-port.txt` to `3-HTTP/port.txt`
and then run `make build && ./restart-service.sh --dev` to start all services in dev mode.

This readme can be generated via `make readme`.

### Frameworks used
* [C++](https://de.wikipedia.org/wiki/C%2B%2B) for parsing and evaluating MiliLISP
* [Haskell](https://www.haskell.org/) with
  * [Firefly](https://hackage.haskell.org/package/firefly) which exposes the interpreter via REST API at port [3-HTTP/port.txt](3-HTTP/port.txt)
  * as well as for simple scripting contexts
* [NodeJS](https://nodejs.org/en/) and [Vue 3](https://v3.vuejs.org/) for frontend scratchpad with [Prism](https://prismjs.com/) for syntax highlighting
* and finally [Docker](https://www.docker.com/) to package everything into deployable images

### Example expressions
* :)  `'(a b c) => (a b c)`
* :)  `'(a ) => (a)`
* :)  `'(a (b c)) => (a (b c))`
* :)  `'(a(b c)) => (a (b c))`
* :)  `'((b c)a) => ((b c) a)`
* :)  `'((b c) a) => ((b c) a)`
* :)  `(quote (a b c)) => (a b c)`
* :)  `'sdf => sdf`
* :)  `'(a b c) => (a b c)`
* :)  `'a => a`
* :)  `'sdfsr => sdfsr`
* :)  `'('d 'a 1) => ((quote d) (quote a) 1)`
* :)  `'213 => 213`
* :)  `(quote (23 dfs 34)) => (23 dfs 34)`
* :)  `(+ '2 '3 '4) => 9`
* :)  `'() => ()`
* :)  `'t => t`
* :)  `(decr '3) => 2`
* :)  `(atom 'a) => t`
* :)  `(atom '3) => t`
* :)  `(atom '(a b c)) => ()`
* :)  `(eq '1 '1) => t`
* :)  `(eq '1 '2) => ()`
* :)  `(eq 'a 'b) => ()`
* :)  `(eq 'a 'a) => t`
* :)  `(eq '() 'a) => ()`
* :)  `(eq '() '()) => t`
* :)  `(eq '(a) '()) => ()`
* :)  `(car '(a)) => a`
* :)  `(car '(a b c)) => a`
* :)  `(car '((a b) d e)) => (a b)`
* :)  `(cdr '(a)) => ()`
* :)  `(cdr '(a b c)) => (b c)`
* :)  `(cdr '((a b) (d e) f)) => ((d e) f)`
* :)  `(cons 'a '(b c)) => (a b c)`
* :)  `(cons 'a '()) => (a)`
* :)  `(cons '(a b) '(d e)) => ((a b) d e)`
* :)  `(cond ('t 'a)) => a`
* :)  `(cond ('() 'b) ('t 'a)) => a`
* :)  `(cond ('(s) 'a) ('() 'b) ('t 'c)) => c`
* :)  `(cond ((eq 'a 'b) 'a) ('t 'b)) => b`
* :)  `(cond ((eq 'a 'a) 'a) ('t 'b)) => a`
* :)  `((cond ('() '+) ('t 'car)) '(a b c)) => a`
* :)  `((cond ('() '+) ('t 'quote)) (a b c)) => (a b c)`
* :)  `(list) => ()`
* :)  `(list 'a) => (a)`
* :)  `(list 'a 'b) => (a b)`
* :)  `((lambda () 'a)) => a`
* :)  `((lambda () (cons '1 '(c d)))) => (1 c d)`
* :)  `((lambda (a b) (cons a b)) '1 '(c d)) => (1 c d)`
* :)  `((lambda (a) (cons a '(b c))) '1) => (1 b c)`
* :)  `((lambda (a) (cons a '(a c))) '1) => (1 a c)`
* :)  `((lambda (a) 'a) '1) => a`
* :)  `((lambda (a b) (+ (car a) b)) '(1 x) '2) => 3`
* :)  `((lambda (a b) a) 'a lambda-is-lazy) => a`
* :)  `((lambda (a b) b) lambda-is-lazy 'a) => a`
* :)  `((lambda (x) (cons x (cons x '()))) 'a) => (a a)`
* :)  `((lambda (f x) (cons f (cons f (cons x '())))) 'f 'x) => (f f x)`
* :)  `((lambda (f xs) (cons f (cons f xs))) 'f '(arg1 arg2)) => (f f arg1 arg2)`
* :)  `((lambda (a b ig) (cons a (cons b ((lambda (b c) (cons b (cons c '()))) '3 '4))) ) '1 '2 '#ignore-embed-eval#) => (1 2 3 4)`
* :)  `((lambda (f n ig) (cond ((eq n '0) '0) ('t (+ n (f (decr n) ))))) '+ '2 '#ignore-embed-eval#) => 3`
* :)  `( (lambda (f x z) (f f x)) '(lambda (f n) (cond ((eq n '0) '0) ('t (+ n (f f (decr n)))))) '3  '#ignore-embed-eval#) => 6`
* :)  `test-var => test-value`
* :)  `U-comb => (lambda (f x) (f f x))`
* :)  `(define a 'b (define s (car '(1 2 3)) (list a s) ) ) => (b 1)`
* :)  `quote => #primitive-quote#`
* :)  `atom => #primitive-atom#`
* :)  `(null 'a) => ()`
* :)  `(null '()) => t`
* :)  `'#ignore-embed-eval-following# => #ignore-embed-eval-following#`
* :)  `(ifelse 't 'a 'b) => a`
* :)  `(ifelse '() 'a 'b) => b`
* :)  `(and (null '()) (eq 'a 'a)) => t`
* :)  `(and (atom 'a) (eq 'a 'b)) => ()`
* :)  `(len '()) => 0`
* :)  `(len '(a)) => 1`
* :)  `(len '(a b)) => 2`
* :)  `(unlist 'a 'b '()) => a`
* :)  `(unlist 'a 'cons '(a b)) => (a b)`
* :)  `(append '(a b) '(c d)) => (a b c d)`
* :)  `(append '() '(c d)) => (c d)`
* :)  `(append '() '()) => ()`
* :)  `(append '(a b) '()) => (a b)`
* :)  `(foldr 'cons '() '()) => ()`
* :)  `(foldr 'cons '() '(a)) => (a)`
* :)  `(foldr 'cons '() '(a b)) => (a b)`
* :)  `(foldl 'cons '1 '((a) (b c))) => ((1 a) b c)`
* :)  `(zip '() '()) => ()`
* :)  `(zip '(a) '()) => ()`
* :)  `(zip '(a) '(1 2)) => ((a 1))`
* :)  `(zip '(a b) '(1 2)) => ((a 1) (b 2))`
* :)  `(assoc 'a '((a 1) (b 2)) '#) => 1`
* :)  `(assoc 'b '((a 1) (b 2)) '#) => 2`
* :)  `(assoc 'c '((a 1) (b 2)) '#) => #`
* :)  `(map 'decr '(1 2 3)) => (0 1 2)`
* :)  `(is-quote ''abc) => t`
* :)  `(is-quote 'abc) => ()`
* :)  `(is-quote '(a b c)) => ()`
* :)  `(is-quote ''(a b c)) => t`
* :)  `(lambda-args '(lambda (a b c) a)) => (a b c)`
* :)  `(lambda-args '(lambda () a)) => ()`
* :)  `(lambda-args '(a b c)) => ()`
* :)  `(contains '(a b c) 'a) => t`
* :)  `(contains '(a b c) '0) => ()`
* :)  `(substitute 'a 'b 'a) => b`
* :)  `(substitute 'a 'b 'c) => c`
* :)  `(substitute 'a '(0 1) '(0 a a)) => (0 (0 1) (0 1))`
* :)  `(substitute 'a '(0 1) ''(0 a a)) => (quote (0 a a))`
* :)  `(substitute 'a '(0 1) '(lambda (0 a b) (list 0 a b))) => (lambda (0 a b) (list 0 a b))`
* :)  `(substitute 'list 'd '(lambda (0 a b) (list 0 a b))) => (lambda (0 a b) (d 0 a b))`
* :)  `(define expr '(+ '1 '2 '3) (eval expr '())) => 6`
* :)  `(define expr '(+ '1 '2 '3) (eval (set-head 'list expr) '())) => (1 2 3)`
* :)  `(eval 'a '()) => (error undefined-atom a)`
* :)  `(eval 'a '((a 1))) => 1`
* :)  `(eval '() '()) => (error empty-list-eval)`
* :)  `(eval ''a '()) => a`
* :)  `(eval '(atom 'a) '()) => t`
* :)  `(eval '(atom '()) '()) => ()`
* :)  `(eval '(eq 'a 'a) '()) => t`
* :)  `(eval '(eq 'a 'b) '()) => ()`
* :)  `(eval '(car '(a b)) '()) => a`
* :)  `(eval '(cdr '(a b)) '()) => (b)`
* :)  `(eval '(cond ('t 'a)) '()) => a`
* :)  `(eval '(cond ('() 'b) ('t 'a)) '()) => a`
* *************`
* Repeating tests with lisp self-interpreter: expr -> (eval 'expr (environment))`
* :)  ` (eval ''(a b c) (environment)) => (a b c)`
* :)  ` (eval ''(a ) (environment)) => (a)`
* :)  ` (eval ''(a (b c)) (environment)) => (a (b c))`
* :)  ` (eval ''(a(b c)) (environment)) => (a (b c))`
* :)  ` (eval ''((b c)a) (environment)) => ((b c) a)`
* :)  ` (eval ''((b c) a) (environment)) => ((b c) a)`
* :)  ` (eval '(quote (a b c)) (environment)) => (a b c)`
* :)  ` (eval ''sdf (environment)) => sdf`
* :)  ` (eval ''(a b c) (environment)) => (a b c)`
* :)  ` (eval ''a (environment)) => a`
* :)  ` (eval ''sdfsr (environment)) => sdfsr`
* :)  ` (eval ''('d 'a 1) (environment)) => ((quote d) (quote a) 1)`
* :)  ` (eval ''213 (environment)) => 213`
* :)  ` (eval '(quote (23 dfs 34)) (environment)) => (23 dfs 34)`
* :)  ` (eval '(+ '2 '3 '4) (environment)) => 9`
* :)  ` (eval ''() (environment)) => ()`
* :)  ` (eval ''t (environment)) => t`
* :)  ` (eval '(decr '3) (environment)) => 2`
* :)  ` (eval '(atom 'a) (environment)) => t`
* :)  ` (eval '(atom '3) (environment)) => t`
* :)  ` (eval '(atom '(a b c)) (environment)) => ()`
* :)  ` (eval '(eq '1 '1) (environment)) => t`
* :)  ` (eval '(eq '1 '2) (environment)) => ()`
* :)  ` (eval '(eq 'a 'b) (environment)) => ()`
* :)  ` (eval '(eq 'a 'a) (environment)) => t`
* :)  ` (eval '(eq '() 'a) (environment)) => ()`
* :)  ` (eval '(eq '() '()) (environment)) => t`
* :)  ` (eval '(eq '(a) '()) (environment)) => ()`
* :)  ` (eval '(car '(a)) (environment)) => a`
* :)  ` (eval '(car '(a b c)) (environment)) => a`
* :)  ` (eval '(car '((a b) d e)) (environment)) => (a b)`
* :)  ` (eval '(cdr '(a)) (environment)) => ()`
* :)  ` (eval '(cdr '(a b c)) (environment)) => (b c)`
* :)  ` (eval '(cdr '((a b) (d e) f)) (environment)) => ((d e) f)`
* :)  ` (eval '(cons 'a '(b c)) (environment)) => (a b c)`
* :)  ` (eval '(cons 'a '()) (environment)) => (a)`
* :)  ` (eval '(cons '(a b) '(d e)) (environment)) => ((a b) d e)`
* :)  ` (eval '(cond ('t 'a)) (environment)) => a`
* :)  ` (eval '(cond ('() 'b) ('t 'a)) (environment)) => a`
* :)  ` (eval '(cond ('(s) 'a) ('() 'b) ('t 'c)) (environment)) => c`
* :)  ` (eval '(cond ((eq 'a 'b) 'a) ('t 'b)) (environment)) => b`
* :)  ` (eval '(cond ((eq 'a 'a) 'a) ('t 'b)) (environment)) => a`
* :)  ` (eval '((cond ('() '+) ('t 'car)) '(a b c)) (environment)) => a`
* :)  ` (eval '((cond ('() '+) ('t 'quote)) (a b c)) (environment)) => (a b c)`
* :)  ` (eval '(list) (environment)) => ()`
* :)  ` (eval '(list 'a) (environment)) => (a)`
* :)  ` (eval '(list 'a 'b) (environment)) => (a b)`
* :)  ` (eval '((lambda () 'a)) (environment)) => a`
* :)  ` (eval '((lambda () (cons '1 '(c d)))) (environment)) => (1 c d)`
* :)  ` (eval '((lambda (a b) (cons a b)) '1 '(c d)) (environment)) => (1 c d)`
* :)  ` (eval '((lambda (a) (cons a '(b c))) '1) (environment)) => (1 b c)`
* :)  ` (eval '((lambda (a) (cons a '(a c))) '1) (environment)) => (1 a c)`
* :)  ` (eval '((lambda (a) 'a) '1) (environment)) => a`
* :)  ` (eval '((lambda (a b) (+ (car a) b)) '(1 x) '2) (environment)) => 3`
* :)  ` (eval '((lambda (a b) a) 'a lambda-is-lazy) (environment)) => a`
* :)  ` (eval '((lambda (a b) b) lambda-is-lazy 'a) (environment)) => a`
* :)  ` (eval '((lambda (x) (cons x (cons x '()))) 'a) (environment)) => (a a)`
* :)  ` (eval '((lambda (f x) (cons f (cons f (cons x '())))) 'f 'x) (environment)) => (f f x)`
* :)  ` (eval '((lambda (f xs) (cons f (cons f xs))) 'f '(arg1 arg2)) (environment)) => (f f arg1 arg2)`
* SKIP  ` (eval '((lambda (a b ig) (cons a (cons b ((lambda (b c) (cons b (cons c '()))) '3 '4))) ) '1 '2 '#ignore-embed-eval#) (environment)) => (1 2 3 4), due to #ignore-embed-eval#`
* SKIP  ` (eval '((lambda (f n ig) (cond ((eq n '0) '0) ('t (+ n (f (decr n) ))))) '+ '2 '#ignore-embed-eval#) (environment)) => 3, due to #ignore-embed-eval#`
* SKIP  ` (eval '( (lambda (f x z) (f f x)) '(lambda (f n) (cond ((eq n '0) '0) ('t (+ n (f f (decr n)))))) '3  '#ignore-embed-eval#) (environment)) => 6, due to #ignore-embed-eval#`
* :)  ` (eval 'test-var (environment)) => test-value`
* :)  ` (eval 'U-comb (environment)) => (lambda (f x) (f f x))`
* :)  ` (eval '(define a 'b (define s (car '(1 2 3)) (list a s) ) ) (environment)) => (b 1)`
* :)  ` (eval 'quote (environment)) => #primitive-quote#`
* :)  ` (eval 'atom (environment)) => #primitive-atom#`
* :)  ` (eval '(null 'a) (environment)) => ()`
* :)  ` (eval '(null '()) (environment)) => t`
* *+++ All tests passed! +++*`
