(define! test-var 'test-value)
(define! ifelse '(lambda (b x y) (cond (b x) ('t y))))
(define! U-comb '(lambda (f x) (f f x)))
(define! null '(lambda (x) (eq x '())) )
(define! not '(lambda (x) (ifelse x '() 't)) )
(define! and '(lambda (a b) (ifelse a (ifelse b 't '()) '())) )
(define! or '(lambda (a b) (not (and (not a) (not b)))))
(define! len '(lambda (xs) (ifelse (null xs) '0  (+ '1 (len (cdr xs))))))
(define! unlist '(lambda (z f xs) (ifelse (null xs) z (f (car xs) (cdr xs)))))
(define! foldr '(lambda (f z xs) (unlist z (lambda (x rs) (f x (foldr f z rs))) xs)))
(define! foldl '(lambda (f z xs) (unlist z (lambda (x rs) (foldl f (f z x) rs)) xs)))
(define! append '(lambda (xs ys) (unlist ys (lambda (x rs) (cons x (append rs ys))) xs)))
(define! zip '(lambda (xs ys) (ifelse (or (null xs) (null ys)) '()  (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys))))))
(define! caar '(lambda (x) (car (car x))))
(define! cadr '(lambda (x) (car (cdr x))))
(define! cadar '(lambda (x) (car (cdr (car x)))))
(define! assoc \
  '(lambda (k ps nl) \
    (cond \
      ((null ps) nl) \
      ((eq k (caar ps)) (cadar ps)) \
      ('t (assoc k (cdr ps) nl)) \
    ) \
  ) \
)
(define! map '(lambda (f xs) (foldr (lambda (x r) (cons (f x) r)) '() xs)))
(define! is_quote '(lambda (e) (and (not (atom e)) (eq (car e) 'quote)) ))
(define! substitute '(lambda (var val body) \
  (ifelse \
    (atom body) (ifelse (eq body var) val body) \
    (map \
      (lambda (x) '???) \
    body) \
  ) \
))
(define! eval \
  '(lambda (e env) \
    (ifelse \
      (atom e) (assoc e env (list 'error 'undefined-atom e)) \
      (unlist '(error empty-list-eval) \
        (lambda (hd tl) \
          (ifelse \
            (atom hd) \
              (cond \
                ((eq hd 'quote) (car tl)) \
                ((eq hd 'atom) (atom (eval (car tl) env))) \
                ((eq hd 'eq) (eq (eval (car tl) env) (eval (cadr tl) env))) \
                ((eq hd 'car) (car (eval (car tl) env))) \
                ((eq hd 'cdr) (cdr (eval (car tl) env))) \
                ((eq hd 'cons) (cons (eval (car tl) env) (eval (cadr tl) env))) \
                ((eq hd 'list) (ifelse (null tl) '() (cons (eval (car tl) env) (eval (cons 'list (cdr tl)) env)))) \
                ((eq hd 'cond) \
                  (ifelse (eval (caar tl) env) (eval (cadar tl) env) \
                  (eval (cons 'cond (cdr tl)) env))) \
                ((eq hd '+) \
                  (ifelse (null tl) '0 (+ (eval (car tl) env) \
                  (eval (cons '+ (cdr tl)) env)))) \
                ((eq hd 'decr) (decr (eval (car tl) env))) \
                ('t (ifelse \
                      (null (assoc hd env '())) \
                        (list 'error 'cannot-eval-atom hd) \
                      (eval (cons (assoc hd env '()) tl) env) \
                    )) \
              ) \
            (ifelse \
              (and (not (null hd)) (eq (car hd) 'lambda)) \
                (foldl \
                  (lambda (acc x) '(substitute to be implemented)) \
                  (caddr hd) \
                  (cadr hd)) \
              (eval (cons (eval hd env) tl) env)) \
          ) \
        ) \
       e) \
    ) \
  ) \
)
