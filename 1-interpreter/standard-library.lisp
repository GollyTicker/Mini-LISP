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
(define! head 'car)
(define! tail 'cdr)

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

; helper functions for eval
(define! is-quote '(lambda (e) (and (not (atom e)) (eq (car e) 'quote))))
(define! lambda-args '(lambda (e)
  (ifelse (and (not (atom e)) (eq (car e) 'lambda)) (cadr e) '())
))
(define! contains '(lambda (xs x) (cond
  ((null xs) '())
  ((eq (car xs) x) 't)
  ('t (contains (cdr xs) x))
)))
(define! substitute '(lambda (var val body)
  (cond
    ((atom body) (ifelse (eq body var) val body))
    ((is-quote body) body)
    ((contains (lambda-args body) var) body)
    ('t (map (lambda (a) (substitute var val a)) body))
    ('t 'TODO-define!-non-substitution-not-implemented)
  )
))

; eval
(define! eval-lambda '(lambda (body args inps env)
  (eval (foldl
    (lambda (acc kv) (substitute (car kv) (cadr kv) acc))
    body
    (zip args inps))
  env)
))
(define! eval
  '(lambda (e env)
    (ifelse
      (atom e) (assoc e env (list 'error 'undefined-atom e))
      (unlist '(error empty-list-eval)
        (lambda (hd tl)
          (ifelse
            (atom hd)
              (cond
                ((eq hd 'quote) (car tl))
                ((eq hd 'atom) (atom (eval (car tl) env)))
                ((eq hd 'eq) (eq (eval (car tl) env) (eval (cadr tl) env)))
                ((eq hd 'car) (car (eval (car tl) env)))
                ((eq hd 'cdr) (cdr (eval (car tl) env)))
                ((eq hd 'cons) (cons (eval (car tl) env) (eval (cadr tl) env)))
                ((eq hd 'list) (ifelse (null tl) '() (cons (eval (car tl) env) (eval (cons 'list (cdr tl)) env))))
                ((eq hd 'cond)
                  (ifelse (eval (caar tl) env) (eval (cadar tl) env)
                  (eval (cons 'cond (cdr tl)) env)))
                ((eq hd 'define) (eval (substitute (car tl) (cadr tl) (caddr tl)) env))
                ((eq hd '+)
                  (ifelse (null tl) '0 (+ (eval (car tl) env)
                  (eval (cons '+ (cdr tl)) env))))
                ((eq hd 'decr) (decr (eval (car tl) env)))
                ('t (ifelse
                      (null (assoc hd env '()))
                        (list 'error 'cannot-eval-atom hd)
                      (eval (cons (assoc hd env '()) tl) env)
                    ))
              )
            (ifelse
              (and (not (null hd)) (eq (car hd) 'lambda))
                (eval-lambda (caddr hd) (cadr hd) tl env)
              (eval (cons (eval hd env) tl) env))
          )
        )
       e)
    )
  )
)

; set-head for homoiconicity example
(define! set-head '(lambda (new-head expr)
 (cons new-head (cdr expr))
))
