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
* *memory management and garbage collection*

The mini-LISP interpreter implements a larger LISP additionally containing:
* `quasiquotation`
* (`algebraic data types`)

How to run:
* installation: *will be documented...*
* `./generate-readme.sh` to run the compile and run the interpreter on the lines in `example.in` and copy their result into the readme.

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
* :)  `((lambda (a b) (cons a (cons b ((lambda (b c) (cons b (cons c '()))) '3 '4))) ) '1 '2) => (1 2 3 4)`
* :)  `((lambda (a b) (+ (car a) b)) '(1 x) '2) => 3`
* :)  `((lambda (a b) a) 'a lambda-is-lazy) => a`
* :)  `((lambda (a b) b) lambda-is-lazy 'a) => a`
* :)  `((lambda (x) (cons x (cons x '()))) 'a) => (a a)`
* :)  `((lambda (f x) (cons f (cons f (cons x '())))) 'f 'x) => (f f x)`
* :)  `((lambda (f xs) (cons f (cons f xs))) 'f '(arg1 arg2)) => (f f arg1 arg2)`
* :)  `((lambda (f n) (cond ((eq n '0) '0) ('t (+ n (f (decr n)))))) '+ '2) => 3`
* :)  `( (lambda (f x) (f f x)) '(lambda (f n) (cond ((eq n '0) '0) ('t (+ n (f f (decr n)))))) '3) => 6`
* :)  `test-var => test-value`
* :)  `U-comb => (lambda (f x) (f f x))`
* :)  `(null 'a) => ()`
* :)  `(null '()) => t`
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
* XX  ` (eval '((lambda () 'a)) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '((lambda () (cons '1 '(c d)))) (environment)) => (1 c d), but got: lambda-not-implemented`
* XX  ` (eval '((lambda (a b) (cons a b)) '1 '(c d)) (environment)) => (1 c d), but got: lambda-not-implemented`
* XX  ` (eval '((lambda (a) (cons a '(b c))) '1) (environment)) => (1 b c), but got: lambda-not-implemented`
* XX  ` (eval '((lambda (a) (cons a '(a c))) '1) (environment)) => (1 a c), but got: lambda-not-implemented`
* XX  ` (eval '((lambda (a) 'a) '1) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '((lambda (a b) (cons a (cons b ((lambda (b c) (cons b (cons c '()))) '3 '4))) ) '1 '2) (environment)) => (1 2 3 4), but got: lambda-not-implemented`
* XX  ` (eval '((lambda (a b) (+ (car a) b)) '(1 x) '2) (environment)) => 3, but got: lambda-not-implemented`
* XX  ` (eval '((lambda (a b) a) 'a lambda-is-lazy) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '((lambda (a b) b) lambda-is-lazy 'a) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '((lambda (x) (cons x (cons x '()))) 'a) (environment)) => (a a), but got: lambda-not-implemented`
* XX  ` (eval '((lambda (f x) (cons f (cons f (cons x '())))) 'f 'x) (environment)) => (f f x), but got: lambda-not-implemented`
* XX  ` (eval '((lambda (f xs) (cons f (cons f xs))) 'f '(arg1 arg2)) (environment)) => (f f arg1 arg2), but got: lambda-not-implemented`
* XX  ` (eval '((lambda (f n) (cond ((eq n '0) '0) ('t (+ n (f (decr n)))))) '+ '2) (environment)) => 3, but got: lambda-not-implemented`
* XX  ` (eval '( (lambda (f x) (f f x)) '(lambda (f n) (cond ((eq n '0) '0) ('t (+ n (f f (decr n)))))) '3) (environment)) => 6, but got: lambda-not-implemented`
* :)  ` (eval 'test-var (environment)) => test-value`
* :)  ` (eval 'U-comb (environment)) => (lambda (f x) (f f x))`
* XX  ` (eval '(null 'a) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(null '()) (environment)) => t, but got: lambda-not-implemented`
* XX  ` (eval '(ifelse 't 'a 'b) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '(ifelse '() 'a 'b) (environment)) => b, but got: lambda-not-implemented`
* XX  ` (eval '(and (null '()) (eq 'a 'a)) (environment)) => t, but got: lambda-not-implemented`
* XX  ` (eval '(and (atom 'a) (eq 'a 'b)) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(len '()) (environment)) => 0, but got: lambda-not-implemented`
* XX  ` (eval '(len '(a)) (environment)) => 1, but got: lambda-not-implemented`
* XX  ` (eval '(len '(a b)) (environment)) => 2, but got: lambda-not-implemented`
* XX  ` (eval '(unlist 'a 'b '()) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '(unlist 'a 'cons '(a b)) (environment)) => (a b), but got: lambda-not-implemented`
* XX  ` (eval '(append '(a b) '(c d)) (environment)) => (a b c d), but got: lambda-not-implemented`
* XX  ` (eval '(append '() '(c d)) (environment)) => (c d), but got: lambda-not-implemented`
* XX  ` (eval '(append '() '()) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(append '(a b) '()) (environment)) => (a b), but got: lambda-not-implemented`
* XX  ` (eval '(foldr 'cons '() '()) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(foldr 'cons '() '(a)) (environment)) => (a), but got: lambda-not-implemented`
* XX  ` (eval '(foldr 'cons '() '(a b)) (environment)) => (a b), but got: lambda-not-implemented`
* XX  ` (eval '(foldl 'cons '1 '((a) (b c))) (environment)) => ((1 a) b c), but got: lambda-not-implemented`
* XX  ` (eval '(zip '() '()) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(zip '(a) '()) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(zip '(a) '(1 2)) (environment)) => ((a 1)), but got: lambda-not-implemented`
* XX  ` (eval '(zip '(a b) '(1 2)) (environment)) => ((a 1) (b 2)), but got: lambda-not-implemented`
* XX  ` (eval '(assoc 'a '((a 1) (b 2)) '#) (environment)) => 1, but got: lambda-not-implemented`
* XX  ` (eval '(assoc 'b '((a 1) (b 2)) '#) (environment)) => 2, but got: lambda-not-implemented`
* XX  ` (eval '(assoc 'c '((a 1) (b 2)) '#) (environment)) => #, but got: lambda-not-implemented`
* XX  ` (eval '(map 'decr '(1 2 3)) (environment)) => (0 1 2), but got: lambda-not-implemented`
* XX  ` (eval '(is-quote ''abc) (environment)) => t, but got: lambda-not-implemented`
* XX  ` (eval '(is-quote 'abc) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(is-quote '(a b c)) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(is-quote ''(a b c)) (environment)) => t, but got: lambda-not-implemented`
* XX  ` (eval '(lambda-args '(lambda (a b c) a)) (environment)) => (a b c), but got: lambda-not-implemented`
* XX  ` (eval '(lambda-args '(lambda () a)) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(lambda-args '(a b c)) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(contains '(a b c) 'a) (environment)) => t, but got: lambda-not-implemented`
* XX  ` (eval '(contains '(a b c) '0) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(substitute 'a 'b 'a) (environment)) => b, but got: lambda-not-implemented`
* XX  ` (eval '(substitute 'a 'b 'c) (environment)) => c, but got: lambda-not-implemented`
* XX  ` (eval '(substitute 'a '(0 1) '(0 a a)) (environment)) => (0 (0 1) (0 1)), but got: lambda-not-implemented`
* XX  ` (eval '(substitute 'a '(0 1) ''(0 a a)) (environment)) => (quote (0 a a)), but got: lambda-not-implemented`
* XX  ` (eval '(substitute 'a '(0 1) '(lambda (0 a b) (list 0 a b))) (environment)) => (lambda (0 a b) (list 0 a b)), but got: lambda-not-implemented`
* XX  ` (eval '(substitute 'list 'd '(lambda (0 a b) (list 0 a b))) (environment)) => (lambda (0 a b) (d 0 a b)), but got: lambda-not-implemented`
* XX  ` (eval '(eval 'a '()) (environment)) => (error undefined-atom a), but got: lambda-not-implemented`
* XX  ` (eval '(eval 'a '((a 1))) (environment)) => 1, but got: lambda-not-implemented`
* XX  ` (eval '(eval '() '()) (environment)) => (error empty-list-eval), but got: lambda-not-implemented`
* XX  ` (eval '(eval ''a '()) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '(eval '(atom 'a) '()) (environment)) => t, but got: lambda-not-implemented`
* XX  ` (eval '(eval '(atom '()) '()) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(eval '(eq 'a 'a) '()) (environment)) => t, but got: lambda-not-implemented`
* XX  ` (eval '(eval '(eq 'a 'b) '()) (environment)) => (), but got: lambda-not-implemented`
* XX  ` (eval '(eval '(car '(a b)) '()) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '(eval '(cdr '(a b)) '()) (environment)) => (b), but got: lambda-not-implemented`
* XX  ` (eval '(eval '(cond ('t 'a)) '()) (environment)) => a, but got: lambda-not-implemented`
* XX  ` (eval '(eval '(cond ('() 'b) ('t 'a)) '()) (environment)) => a, but got: lambda-not-implemented`
* *XXX Some tests failed! XXX*`
