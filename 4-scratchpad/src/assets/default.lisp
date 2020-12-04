; basics
(define! test-var
  'test-value)
(define! ifelse '(lambda (b x y) (cond (b x) ('t y))))
(define! U-comb '(lambda (f x) (f f x)))
(define! null '(lambda (x) (eq x '())) )
(define! not '(lambda (x) (ifelse x '() 't)) )
(define! and '(lambda (a b) (ifelse a (ifelse b 't '()) '())) )
(define! or '(lambda (a b) (not (and (not a) (not b)))))

; list accessors
(define! caar '(lambda (x) (car (car x))))
(define! cadr '(lambda (x) (car (cdr x))))
(define! cadar '(lambda (x) (car (cdr (car x)))))
(define! caddr '(lambda (x) (car (cdr (cdr x)))))

; list utilities
(define! len '(lambda (xs) (ifelse (null xs) '0  (+ '1 (len (cdr xs))))))
(define! unlist '(lambda (z f xs) (ifelse (null xs) z (f (car xs) (cdr xs)))))
(define! foldr '(lambda (f z xs) (unlist z (lambda (x rs) (f x (foldr f z rs))) xs)))
(define! foldl '(lambda (f z xs) (unlist z (lambda (x rs) (foldl f (f z x) rs)) xs)))
(define! append '(lambda (xs ys) (unlist ys (lambda (x rs) (cons x (append rs ys))) xs)))
(define! zip '(lambda (xs ys) (ifelse (or (null xs) (null ys)) '()  (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys))))))
(define! map '(lambda (f xs) (foldr (lambda (x r) (cons (f x) r)) '() xs)))
(define! assoc ; test-comment
  '(lambda (k ps nl)
    (cond
      ((null ps) nl)
      ((eq k (caar ps)) (cadar ps))
      ('t (assoc k (cdr ps) nl))
    )
  )
)
