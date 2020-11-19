#include<map>
#include <algorithm>

typedef map<string,AST*> Env;
typedef AST* evalProc(List*, Env&);

/* perhaps we can make the evaluation
cleaner by adding a procedure type to AST? */

// procedure that evaluates an expression using the environment.
// may change the environment.
AST* eval(AST* expr, Env& env);

#include "EvalPrimitives.cpp"

AST* eval(AST* expr, Env& env) {
  if (debug) cout << "eval: " << expr-> lisp_string() << endl;

  if (expr->is_atom()) {
    Atom* a = dynamic_cast<Atom*>(expr);
    if (env.count(a->str) >= 1) return env[a->str];
    else {
      cout << "Cannot eval atom " << a->lisp_string() << ". Missing definition in env." << endl;
      return NULL;
    }
  }
  else { // expr is list
    List* xs = dynamic_cast<List*>(expr);
    if(!xs->empty) {
      AST* hd = xs->head;
      // head = predefined atom => apply
      // head = atom => lookup atom and apply
      // head = lambda => apply lambda
      // otherwise => evaluate and repeat

      Atom* atom = dynamic_cast<Atom*>(hd);
      if (atom) {
        if (predefs.count(atom->str) >= 1) {
          // cout << "Applying primitive " << atom->str << " on env of size " << env.size() << endl;
          return predefs[atom->str](xs->tail,env); // evaluation of args based on primitives
        }
        else if (env.count(atom->str) >= 1) {
          //List* eagerArgs = evalList(xs->tail, env); // eager evaluation of args
          //cout << "eval "<< atom->str << " with args " << xs->tail->lisp_string() << endl;
          return eval(cons(env[atom->str],xs->tail),env);
        }
        else {
          cout << "Cannot eval undefined atom in " << xs->lisp_string() << endl;
          return NULL;
        }
      }
      List* fn = dynamic_cast<List*>(hd);
      Atom* fname = dynamic_cast<Atom*>(tryhead(fn));
      if (fname && fname->equals(at("lambda"))) { /*fn is lambda */
        AST* args = tryhead(trytail(fn));
        AST* body = tryhead(trytail(trytail(fn)));
        return prim_lambda_apply(args,body,xs->tail,env);
      }
      else { /* evaluate fn and repeat */
        AST* evaledfn = eval(fn, env);
        // defer evaluation of arguments to next eval
        return eval(cons(evaledfn,xs->tail),env);
      }
    }
    else { // empty list
      cout << "Cannot eval empty list " << xs->lisp_string() << "." << endl;
      return NULL;
    }
  }

  return NULL;
}

#define RUN(prog,e) eval(parse_full((prog)),(e))

/* standard library definitions */
void add_standard_library(Env& env) {
  /* TODO: define should be used as search and replace, but now, it's being stronger
    and it's evaluated first! What if we don't use the quotation?
    We could use non-quoted definitions for metaprogramming!*/
  RUN("(define! test-var 'test-value)", env);
  RUN("(define! ifelse '(lambda (b x y) (cond (b x) ('t y))))",env);
  RUN("(define! U-comb '(lambda (f x) (f f x)))", env);
  RUN("(define! null '(lambda (x) (eq x '())) )", env);
  RUN("(define! not '(lambda (x) (ifelse x '() 't)) )", env);
  RUN("(define! and '(lambda (a b) (ifelse a (ifelse b 't '()) '())) )", env);
  RUN("(define! or '(lambda (a b) (not (and (not a) (not b)))))",env);
  RUN("(define! len '(lambda (xs) (ifelse (null xs) '0  (+ '1 (len (cdr xs))))))", env);
  RUN("(define! unlist '(lambda (z f xs) (ifelse (null xs) z (f (car xs) (cdr xs)))))", env);
  RUN("(define! foldr '(lambda (f z xs) (unlist z (lambda (x rs) (f x (foldr f z rs))) xs)))", env);
  RUN("(define! append '(lambda (xs ys) (unlist ys (lambda (x rs) (cons x (append rs ys))) xs)))",env);
  RUN("(define! zip '(lambda (xs ys) (ifelse (or (null xs) (null ys)) '()  (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys))))))",env);
  RUN("(define! caar '(lambda (x) (car (car x))))",env);
  RUN("(define! cadr '(lambda (x) (car (cdr x))))",env);
  RUN("(define! cadar '(lambda (x) (car (cdr (car x)))))",env);
  RUN("(define! assoc "
        "'(lambda (k ps nl) "
          "(cond "
            "((null ps) nl) "
            "((eq k (caar ps)) (cadar ps)) "
            "('t (assoc k (cdr ps) nl))"
          ")"
        ")"
      ")",env);

  // multiline string: https://stackoverflow.com/questions/1135841/c-multiline-string-literal
  string eval_def =
    "(define! eval "
      "'(lambda (e env) "
        "(ifelse "
          "(atom e) (assoc e env (list 'error 'undefined-atom e)) "
          "(unlist '(error empty-list-eval) "
          " (lambda (hd tl)"
            "(cond"
              "((eq hd 'quote) (car tl))"
              "((eq hd 'atom) (atom (eval (car tl) env)))"
              "((eq hd 'eq) (eq (eval (car tl) env) (eval (cadr tl) env)))"
              "('t (list 'error 'not-implemented hd))"
            ")"
          " )"
          " e)"
        ")"
      ")"
    ")";

  RUN(eval_def,env);
}


AST* Eval(AST* expr) {
  Env e;
  add_standard_library(e);
  return eval(expr, e);
}

/*
lambdas online:

;gnu clisp  2.49.60

(print "Hello, world!")

(print ((lambda () (cons '1 '(c d)))))

(print ((lambda (a b) (cons a b)) '1 '(c d)))

(print ((lambda (x) (cons x (cons x '()))) 'a) )

(print ((lambda (x) (cons x (cons x '()))) (lambda (f x) x)))

; (print (((lambda (x) (cons x (cons x '()))) (lambda (f x) x)) 'a) )

; (print (((lambda (x) (cons x (cons x '()))) (lambda (f x) x)) 'a) )
*/
