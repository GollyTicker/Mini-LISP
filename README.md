# LISP

A mini-LISP interpreter in C++ which itself implements a larger LISP.
Inspired by [The Roots of LISP](http://languagelog.ldc.upenn.edu/myl/llog/jmc.pdf).

*TODO*: make note on the fact, that the implementation is not exactly like gnu clisp etc. implementations.

The C++ implements a mini-LISP interpreter consisting of:
* `quote`
* `atom`
* `eq` (atom equality and empty list equality)
* `car`
* `cdr`
* `cons` (and it's abbreviation `list`)
* `cond`
* `lambda`
* `define!` (recursive binding) (*TODO*: local variable binding `define`)
* `environment` (returns all bindings to re-direct standard library)
* `+` and `decr` (convenience for example numerical functions)
* *memory management and garbage collection*

The mini-LISP interpreter implements a larger LISP additionally containing:
* `quasiquotation` ?

How to run:
* installation: *will be documented...*
* `./generate-readme.sh` to run the compile and run the interpreter on the lines in `example.in` and copy their result into the readme
* a REPL via `./REPL`

Features and optimisations:
* head-first evaluation. expressions where head is computed and hence choses which functionto evaluate:
  * for example `((cond ('() '+) ('t 'car)) '(a b c))` can be run with our interpreter and evaluates to `a`
  * whereas online interpreters such as [this one](https://rextester.com/l/common_lisp_online_compiler) (GNU clisp) cannot evaluate such expressions
* memory management via smart pointers (`shared_ptr` and `weak_ptr`) in `AST.cpp`
* lazy variable bindings in `lambda`s. this allows us to define and express recursive functions via logic-combinators. This isn't possible with the default GNU clisp.
  * for example, we can run `(lambda (f x) (f f x)) '(lambda (f n) (cond ((eq n '0) '0) ('t (+ n (f f (decr n)))))) '3)` to compute the sum of the first three integers
  * We cannot use a definition in this form in GNU clisp due to it's head being itself a function.
* since expressions are purely functional (`define!` is only to be used for top-level definitions), we can *memoise* results of evaluations in `Eval.cpp`
  * in complex and long running operations, 13% to 18% of all expressions can be simply looked up.
  * this optimisation isn't implemented in the interpreter written in Lisp itself,
  hence complex test-cases which would take long to execute are excluded during testing.
* a small standard library can be found in `standard-library.lisp`
* improvements from C++: `g++ -O3 ...`

Here are some examples:
* :)  `'(a b c) => (a b c)`
* :)  `'(a ) => (a)`
* :)  `'(a (b c)) => (a (b c))`
* :)  `'(a(b c)) => (a (b c))`
* :)  `'((b c)a) => ((b c) a)`
* :)  `'((b   c) a) => ((b c) a)`
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
* :)  ` (eval ''((b   c) a) (environment)) => ((b c) a)`
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
* :)  ` (eval '(null 'a) (environment)) => ()`
* :)  ` (eval '(null '()) (environment)) => t`
* *+++ All tests passed! +++*`
